(in-package #:spec-backend-quint)

;;; Drive the quint CLI (https://quint.sh) through process-protocol.
;;;
;;;   QUINT_BIN    quint executable (default: "quint" on PATH)
;;;   JAVA_HOME    JDK >= 17 for verify (Apalache / TLC); its bin/ is put first on PATH
;;;   TLA2TOOLS    tla2tools.jar, informational (verify --backend tlc uses Apalache's bundled TLC)
;;;
;;; Verified against quint 0.32.0 / Apalache 0.56.1: --main must name the instance module when
;;; the spec has consts; `quint test` matches only `.*Test` by default; --seed alone caps
;;; --max-samples at 1; ITF variable names are namespaced (spec-protocol.itf strips them).

(defvar *quint-program* (or (uiop:getenv "QUINT_BIN") "quint"))
(defvar *java-home* (uiop:getenv "JAVA_HOME"))
(defvar *tla2tools* (uiop:getenv "TLA2TOOLS"))

(defclass quint-backend (spec-backend)
  ((program :initarg :program :initform *quint-program* :reader backend-program)
   (directory :initarg :directory :initform nil :reader backend-directory
              :documentation "Work directory; NIL = fresh temp directory per call")
   (keep-files :initarg :keep-files :initform nil :reader backend-keep-files)))

(defun make-quint-backend (&rest initargs)
  (apply #'make-instance 'quint-backend initargs))

(defun use-quint-backend (&rest initargs)
  (setf *spec-backend* (apply #'make-quint-backend initargs)))

;;; Process plumbing

(defun %text (octets)
  (if (and octets (plusp (length octets))) (babel:octets-to-string octets :encoding :utf-8) ""))

(defun %environment-with-java ()
  "Current environment with JAVA_HOME/bin first on PATH, or NIL to inherit unchanged."
  (when *java-home*
    (let* ((bin (format nil "~a/bin" (string-right-trim "/" *java-home*)))
           (env #+sbcl (sb-ext:posix-environ) #-sbcl nil))
      (when env
        (cons (format nil "PATH=~a:~a" bin (or (uiop:getenv "PATH") ""))
              (cons (format nil "JAVA_HOME=~a" *java-home*)
                    (remove-if (lambda (s) (or (uiop:string-prefix-p "PATH=" s) (uiop:string-prefix-p "JAVA_HOME=" s)))
                               env)))))))

(defun run-quint (backend subcommand args &key directory java)
  "-> (values exit-code stdout stderr). Signals SPEC-TOOL-MISSING-ERROR when quint cannot start."
  (let ((command (append (list (backend-program backend) subcommand) args)))
    (handler-case
        (multiple-value-bind (code out err)
            (apply #'process-protocol:run command
                   (append (when directory (list :directory directory))
                           (when java (let ((env (%environment-with-java))) (when env (list :env env))))))
          (when (eql code 127)
            (error 'spec-tool-missing-error :program (backend-program backend) :message (%text err)))
          (values code (%text out) (%text err)))
      (process-protocol:process-error (c)
        (error 'spec-tool-missing-error :program (backend-program backend)
                                        :message (process-protocol:process-error-message c))))))

(defun quint-version (&optional (backend (or *spec-backend* (make-quint-backend))))
  (handler-case
      (multiple-value-bind (code out) (run-quint backend "--version" '())
        (and (eql code 0) (string-trim '(#\Space #\Newline #\Return) out)))
    (spec-tool-missing-error () nil)))

(defun quint-available-p (&optional (backend (or *spec-backend* (make-quint-backend))))
  (and (quint-version backend) t))

(defun java-available-p ()
  (and *java-home* (probe-file (format nil "~a/bin/java" (string-right-trim "/" *java-home*))) t))

;;; Work directory + files

(defun %call-with-workdir (backend fn)
  (if (backend-directory backend)
      (funcall fn (uiop:ensure-directory-pathname (backend-directory backend)))
      (let ((dir (cl-stack-pathlib:path-pathname (cl-stack-pathlib:make-temp-directory :prefix "spec-quint-"))))
        (unwind-protect (funcall fn (uiop:ensure-directory-pathname dir))
          (unless (backend-keep-files backend)
            (ignore-errors (cl-stack-pathlib:rmtree dir :missing-ok t)))))))

(defmacro with-workdir ((dir backend) &body body)
  `(%call-with-workdir ,backend (lambda (,dir) ,@body)))

(defun %write-spec (module dir)
  (let ((file (merge-pathnames (format nil "~a.qnt" (module-name module)) dir)))
    (with-open-file (out file :direction :output :if-exists :supersede :external-format :utf-8)
      (emit-module module out))
    file))

(defun %read-json (pathname)
  (when (probe-file pathname)
    (with-open-file (in pathname :external-format :utf-8)
      (let* ((text (make-string (file-length in)))
             (n (read-sequence text in)))
        (json-protocol:decode (subseq text 0 n))))))

(defun %json-errors (obj)
  "ADR-002 errors -> list of strings."
  (when (hash-table-p obj)
    (let ((errors (gethash "errors" obj)))
      (when (vectorp errors)
        (map 'list (lambda (e)
                     (let ((explanation (gethash "explanation" e))
                           (locs (gethash "locs" e)))
                       (if (and (vectorp locs) (plusp (length locs)))
                           (let* ((loc (aref locs 0)) (start (gethash "start" loc)))
                             (format nil "~a (~a:~a)" (string-right-trim '(#\Newline) explanation)
                                     (gethash "source" loc) (and start (1+ (gethash "line" start)))))
                           (string-right-trim '(#\Newline) explanation))))
             errors)))))

(defun %status (obj code)
  (let ((status (and (hash-table-p obj) (gethash "status" obj))))
    (cond ((equal status "ok") :ok)
          ((equal status "violation") :violation)
          ((and (null status) (eql code 0)) :ok)
          (t :error))))

(defun %result (obj code out err &key (tool :quint) traces)
  (make-instance 'spec-result
                 :status (%status obj code) :tool tool :stdout out :stderr err
                 :errors (%json-errors obj)
                 :seed (and (hash-table-p obj) (let ((s (gethash "seed" obj))) (and (integerp s) s)))
                 :traces traces))

(defun %flag (name value)
  (when value (list name (if (stringp value) value (princ-to-string value)))))

(defun %names (module names &optional (decls (module-invariants module)))
  "Spec names for NAMES (symbols / strings). :ALL and NIL mean all invariant decls."
  (cond ((or (null names) (eq names :all)) (mapcar #'decl-name decls))
        (t (mapcar (lambda (n) (if (stringp n)
                                   n
                                   (let ((d (module-find-decl module n)))
                                     (if d (decl-name d) (mangle-name n)))))
                   names))))

(defun %load-traces (dir)
  (mapcar #'spec-protocol.itf:read-itf
          (sort (directory (merge-pathnames "trace_*.itf.json" dir)) #'string< :key #'namestring)))

;;; Backend protocol

(defmethod backend-emit ((backend quint-backend) module &key stream)
  (if stream
      (progn (emit-module module stream) nil)
      (emit-to-string module)))

(defmethod backend-typecheck ((backend quint-backend) module &key directory)
  (declare (ignore directory))
  (with-workdir (dir backend)
    (let ((file (%write-spec module dir))
          (out-file (merge-pathnames "typecheck.json" dir)))
      (multiple-value-bind (code out err)
          (run-quint backend "typecheck" (list (namestring file) "--out" (namestring out-file)) :directory dir)
        (let ((obj (%read-json out-file)))
          (make-instance 'spec-result :status (if (eql code 0) :ok :error) :tool :quint
                                      :stdout out :stderr err :errors (or (%json-errors obj)
                                                                          (and (/= code 0) (list err)))))))))

(defmethod backend-simulate ((backend quint-backend) module
                             &key init step invariants witnesses (max-steps 20) (max-samples 100)
                               seed (n-traces 1) (mbt t) directory)
  (declare (ignore directory))
  (with-workdir (dir backend)
    (let* ((file (%write-spec module dir))
           (out-file (merge-pathnames "run.json" dir))
           (args (append (list (namestring file) "--main" (module-main-name module)
                               "--out" (namestring out-file)
                               "--out-itf" (namestring (merge-pathnames "trace_{seq}.itf.json" dir))
                               "--max-steps" (princ-to-string max-steps)
                               "--max-samples" (princ-to-string (max max-samples n-traces))
                               "--n-traces" (princ-to-string n-traces))
                         (%flag "--init" init) (%flag "--step" step) (%flag "--seed" seed)
                         (let ((names (%names module invariants)))
                           (when names (cons "--invariants" names)))
                         (let ((names (and witnesses (%names module witnesses (module-witnesses module)))))
                           (when names (cons "--witnesses" names)))
                         (when mbt (list "--mbt")))))
      (multiple-value-bind (code out err) (run-quint backend "run" args :directory dir)
        (let ((obj (%read-json out-file)))
          (when (and (null obj) (/= code 0))
            (error 'spec-tool-error :exit-code code :stdout out :stderr err :message "quint run failed"))
          (%result obj code out err :traces (%load-traces dir)))))))

(defmethod backend-verify ((backend quint-backend) module
                           &key init step invariants temporal (max-steps 10) (checker :apalache) directory)
  (declare (ignore directory))
  (with-workdir (dir backend)
    (let* ((file (%write-spec module dir))
           (out-file (merge-pathnames "verify.json" dir))
           (args (append (list (namestring file) "--main" (module-main-name module)
                               "--out" (namestring out-file)
                               "--out-itf" (namestring (merge-pathnames "trace_0.itf.json" dir))
                               "--max-steps" (princ-to-string max-steps))
                         (%flag "--init" init) (%flag "--step" step)
                         (ecase checker (:apalache '()) (:tlc (list "--backend" "tlc")))
                         (let ((names (%names module invariants)))
                           (when names (cons "--invariants" names)))
                         (when temporal
                           (list "--temporal" (format nil "~{~a~^,~}" (%names module temporal (module-ops module))))))))
      (multiple-value-bind (code out err) (run-quint backend "verify" args :directory dir :java t)
        (let ((obj (%read-json out-file)))
          (when (and (null obj) (/= code 0))
            (error 'spec-tool-error :exit-code code :stdout out :stderr err :message "quint verify failed"))
          (%result obj code out err :tool checker :traces (%load-traces dir)))))))

(defmethod backend-run-tests ((backend quint-backend) module &key match seed directory)
  (declare (ignore directory))
  (with-workdir (dir backend)
    (let* ((file (%write-spec module dir))
           (out-file (merge-pathnames "test.json" dir))
           (runs (mapcar #'decl-name (module-runs module)))
           (args (append (list (namestring file) "--main" (module-main-name module)
                               "--out" (namestring out-file)
                               "--match" (or match (format nil "^(~{~a~^|~})$" runs)))
                         (%flag "--seed" seed))))
      (multiple-value-bind (code out err) (run-quint backend "test" args :directory dir)
        (let* ((obj (%read-json out-file))
               (passed (and obj (map 'list #'identity (or (gethash "passed" obj) #()))))
               (failed (and obj (map 'list #'identity (or (gethash "failed" obj) #())))))
          (make-instance 'spec-result
                         :status (cond ((and (eql code 0) (null failed)) :ok) (failed :violation) (t :error))
                         :tool :quint :stdout out :stderr err :errors (%json-errors obj)
                         :passed passed :failed failed))))))

(defmethod backend-compile ((backend quint-backend) module &key (target :tlaplus) directory)
  (declare (ignore directory))
  (with-workdir (dir backend)
    ;; TLA+ goes to stdout (--out would wrap it in the JSON IR envelope) and needs Apalache/Java;
    ;; JSON IR uses --out. cwd is the work directory so Apalache's _apalache-out/ lands there.
    (let* ((file (%write-spec module dir))
           (out-file (merge-pathnames "compiled.json" dir))
           (args (append (list (namestring file) "--main" (module-main-name module)
                               "--target" (string-downcase (symbol-name target)))
                         (when (eq target :json) (list "--out" (namestring out-file))))))
      (multiple-value-bind (code out err) (run-quint backend "compile" args :directory dir :java (eq target :tlaplus))
        (when (/= code 0)
          (error 'spec-tool-error :exit-code code :stdout out :stderr err :message "quint compile failed"))
        (make-instance 'spec-result :status :ok :tool :quint :stdout out :stderr err
                                    :output (ecase target
                                              (:tlaplus (%tla-text out))
                                              (:json (cl-stack-pathlib:read-text out-file))))))))

(defun %tla-text (stdout)
  "Apalache prints a '#'-prefixed log preamble before the TLA+ module; return the module only."
  (let ((start (search "---- MODULE" stdout)))
    (if start (subseq stdout start) stdout)))

(use-quint-backend)
