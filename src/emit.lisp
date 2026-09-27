(in-package #:spec-backend-quint)

;;; spec-protocol AST -> Quint text.
;;;
;;; Actions only state what changes; Quint requires every variable to be assigned in every
;;; step (QNT502), so the emitter adds the frame condition v' = v for untouched variables —
;;; per branch of if / match so every path assigns the same set.

(defvar *indent* 0)
(defvar *module* nil)

(defun %nl (stream) (format stream "~%~v@{ ~}" *indent* nil))
(defmacro with-indent (&body body) `(let ((*indent* (+ *indent* 2))) ,@body))

(defun %name (sym) (mangle-name sym))

(defun %decl-ref (name)
  "Emitted name for a declared operator / var / const, honouring :AS."
  (let ((d (and *module* (module-find-decl *module* name '(not type-decl)))))
    (if d (decl-name d) (%name name))))

;;; Types

(defun emit-type (type stream)
  (etypecase type
    (type-ref (case (type-ref-name type)
                (:int (write-string "int" stream))
                (:str (write-string "str" stream))
                (:bool (write-string "bool" stream))
                (t (write-string (%type-ref-name (type-ref-name type)) stream))))
    (set-type (write-string "Set[" stream) (emit-type (type-elem type) stream) (write-string "]" stream))
    (list-type (write-string "List[" stream) (emit-type (type-elem type) stream) (write-string "]" stream))
    (map-type (write-string "(" stream) (emit-type (type-key type) stream) (write-string " -> " stream)
              (emit-type (type-value type) stream) (write-string ")" stream))
    (tuple-type (write-string "(" stream)
                (loop for (e . rest) on (type-elems type)
                      do (emit-type e stream) (when rest (write-string ", " stream)))
                (write-string ")" stream))
    (record-type (write-string "{ " stream)
                 (loop for ((f . ty) . rest) on (type-fields type)
                       do (format stream "~a: " (%name f)) (emit-type ty stream) (when rest (write-string ", " stream)))
                 (write-string " }" stream))
    (sum-type (loop for ((label . payload) . rest) on (sum-variants type)
                    do (write-string (mangle-type-name label) stream)
                       (when payload (write-string "(" stream) (emit-type payload stream) (write-string ")" stream))
                       (when rest (write-string " | " stream))))))

(defun %type-ref-name (sym)
  (let ((d (and *module* (module-find-decl *module* sym 'type-decl))))
    (if d (decl-name d) (mangle-type-name sym))))

;;; Expressions

(defun %quint-string (s stream)
  (write-char #\" stream)
  (loop for ch across s
        do (case ch
             (#\" (write-string "\\\"" stream))
             (#\\ (write-string "\\\\" stream))
             (#\Newline (write-string "\\n" stream))
             (t (write-char ch stream))))
  (write-char #\" stream))

(defun %args (args stream)
  (loop for (a . rest) on args
        do (emit-expr a stream) (when rest (write-string ", " stream))))

(defun emit-expr (e stream)
  (etypecase e
    (literal (let ((v (literal-value e)))
               (cond ((eq v :true) (write-string "true" stream))
                     ((eq v :false) (write-string "false" stream))
                     ((stringp v) (%quint-string v stream))
                     ((integerp v) (format stream "~d" v))
                     (t (write-string (%name v) stream)))))
    (name-ref (write-string (if (eq (ref-kind e) :param) (%name (ref-name e)) (%decl-ref (ref-name e))) stream))
    (variant-expr (write-string (mangle-type-name (variant-expr-label e)) stream)
                  (when (variant-expr-arg e)
                    (write-string "(" stream) (emit-expr (variant-expr-arg e) stream) (write-string ")" stream)))
    (app (emit-app e stream))
    (lambda-expr
     (let ((params (lambda-params e)))
       (write-string "(" stream)
       (if (= (length params) 1)
           (write-string (%name (first params)) stream)
           (format stream "(~{~a~^, ~})" (mapcar #'%name params)))
       (write-string " => " stream)
       (emit-expr (lambda-body e) stream)
       (write-string ")" stream)))
    (let-expr
     (write-string "{" stream)
     (with-indent
       (loop for (v . x) in (let-bindings e)
             do (%nl stream) (format stream "val ~a = " (%name v)) (emit-expr x stream))
       (%nl stream) (emit-expr (let-body e) stream))
     (%nl stream) (write-string "}" stream))
    (if-expr (write-string "(if (" stream) (emit-expr (if-test e) stream) (write-string ") " stream)
             (emit-expr (if-then e) stream) (write-string " else " stream) (emit-expr (if-else e) stream)
             (write-string ")" stream))
    (match-expr (emit-match e stream #'emit-expr))
    (record-expr (write-string "{ " stream)
                 (loop for ((f . x) . rest) on (record-fields e)
                       do (format stream "~a: " (%name f)) (emit-expr x stream) (when rest (write-string ", " stream)))
                 (write-string " }" stream))
    (field-access (emit-expr (access-record e) stream) (format stream ".~a" (%name (access-field e))))
    (all-expr (if (block-forms e)
                  (progn (write-string "(" stream)
                         (loop for (f . rest) on (block-forms e)
                               do (emit-expr f stream) (when rest (write-string " and " stream)))
                         (write-string ")" stream))
                  (write-string "true" stream)))
    (any-expr (write-string "(" stream)
              (loop for (f . rest) on (block-forms e)
                    do (emit-expr f stream) (when rest (write-string " or " stream)))
              (write-string ")" stream))
    (nondet-expr (emit-nondet e stream #'emit-expr))
    (raw-expr (write-string (raw-text e) stream))
    (assign-expr (format stream "~a' = " (%decl-ref (assign-var e))) (emit-expr (assign-value e) stream))
    (lisp-expr (error 'spec-emit-error :message (format nil "(:lisp …) cannot be emitted outside an action: ~s" (expr-source e))))
    ((or old-expr result-expr)
     (error 'spec-emit-error :message (format nil "~a is not emitted (contract clause only): ~s" (type-of e) (expr-source e))))))

(defun emit-match (e stream body-emitter)
  (write-string "match " stream) (emit-expr (match-scrutinee e) stream) (write-string " {" stream)
  (with-indent
    (loop for (label var body) in (match-clauses e)
          do (%nl stream)
             (if (eq label :default)
                 (write-string "| _ => " stream)
                 (format stream "| ~a~@[(~a)~] => " (mangle-type-name label) (and var (%name var))))
             (funcall body-emitter body stream)))
  (%nl stream) (write-string "}" stream))

(defun emit-nondet (e stream body-emitter)
  "Consecutive nondets share one block. The domain is written as given: (one-of S) -> S.oneOf()."
  (write-string "{" stream)
  (with-indent
    (loop
      (%nl stream) (format stream "nondet ~a = " (%name (nondet-var e))) (emit-expr (nondet-domain e) stream)
      (if (typep (nondet-body e) 'nondet-expr)
          (setf e (nondet-body e))
          (return)))
    (%nl stream) (funcall body-emitter (nondet-body e) stream))
  (%nl stream) (write-string "}" stream))

(defun emit-app (e stream)
  (let* ((op (app-op e)) (args (app-args e)) (b (builtin op)))
    (cond
      (b
       (ecase (builtin-style b)
         (:infix
          (if (and (= (length args) 1) (name= op '-))
              (progn (write-string "(-" stream) (emit-expr (first args) stream) (write-string ")" stream))
              (progn (write-string "(" stream)
                     (loop for (a . rest) on args
                           do (emit-expr a stream) (when rest (format stream " ~a " (builtin-quint b))))
                     (write-string ")" stream))))
         (:prefix (format stream "~a(" (builtin-quint b)) (%args args stream) (write-string ")" stream))
         (:method (emit-expr (first args) stream) (format stream ".~a(" (builtin-quint b))
                  (%args (rest args) stream) (write-string ")" stream))
         (:ctor (format stream "~a(" (builtin-quint b)) (%args args stream) (write-string ")" stream))
         (:list (write-string "[" stream) (%args args stream) (write-string "]" stream))
         (:tuple (write-string "(" stream) (%args args stream) (write-string ")" stream))
         (:map (write-string "Map(" stream)
               (loop for (tup . rest) on args
                     do (emit-expr (first (app-args tup)) stream) (write-string " -> " stream)
                        (emit-expr (second (app-args tup)) stream) (when rest (write-string ", " stream)))
               (write-string ")" stream))
         (:item (emit-expr (first args) stream) (format stream "._~d" (literal-value (second args))))
         (:with (emit-expr (first args) stream)
                (format stream ".with(\"~a\", " (%name (literal-value (second args))))
                (emit-expr (third args) stream) (write-string ")" stream))))
      (t
       (write-string (%decl-ref op) stream)
       (when args (write-string "(" stream) (%args args stream) (write-string ")" stream))))))

;;; Actions: assignment analysis + frame conditions

(defun %action-p (op)
  (let ((d (and *module* (module-find-decl *module* op 'op-def))))
    (and d (eq (op-mode d) :action))))

(defun assigned-vars (e)
  "List of var symbols assigned anywhere in E, or :ALL when E calls another action."
  (labels ((union* (a b) (cond ((or (eq a :all) (eq b :all)) :all) (t (union a b :test #'name=))))
           (walk (e)
             (etypecase e
               (assign-expr (list (assign-var e)))
               (block-expr (reduce #'union* (mapcar #'walk (block-forms e)) :initial-value '()))
               (if-expr (union* (walk (if-then e)) (walk (if-else e))))
               (let-expr (walk (let-body e)))
               (match-expr (reduce #'union* (mapcar (lambda (c) (walk (third c))) (match-clauses e))
                                   :initial-value '()))
               (nondet-expr (walk (nondet-body e)))
               (app (if (%action-p (app-op e)) :all '()))
               (expr '()))))
    (walk e)))

(defun %frame (vars stream &key first)
  "Emit v' = v conjunct lines for VARS."
  (dolist (v vars)
    (unless first (write-string "," stream))
    (setf first nil)
    (%nl stream) (format stream "~a' = ~a" (%decl-ref v) (%decl-ref v))))

(defun emit-action-item (e stream must-assign)
  "Emit one conjunct of an action. MUST-ASSIGN = vars every branch of E has to assign."
  (etypecase e
    (assign-expr (emit-expr e stream))
    (all-expr (emit-all e stream must-assign))
    (if-expr
     (write-string "if (" stream) (emit-expr (if-test e) stream) (write-string ") " stream)
     (emit-branch (if-then e) stream must-assign)
     (write-string " else " stream)
     (emit-branch (if-else e) stream must-assign))
    (match-expr (emit-match e stream (lambda (body s) (emit-branch body s must-assign))))
    (let-expr
     (write-string "{" stream)
     (with-indent
       (loop for (v . x) in (let-bindings e)
             do (%nl stream) (format stream "val ~a = " (%name v)) (emit-expr x stream))
       (%nl stream) (emit-branch (let-body e) stream must-assign))
     (%nl stream) (write-string "}" stream))
    (nondet-expr (emit-nondet e stream (lambda (body s) (emit-branch body s must-assign))))
    (any-expr
     (write-string "any {" stream)
     (with-indent
       (dolist (f (block-forms e))
         (%nl stream) (emit-branch f stream must-assign) (write-string "," stream)))
     (%nl stream) (write-string "}" stream))
    (lisp-expr (write-string "true" stream))
    (expr (emit-expr e stream))))

(defun emit-branch (e stream must-assign)
  "Emit E as a complete branch: its own conjuncts plus frame conditions for MUST-ASSIGN vars it misses."
  (let* ((assigned (assigned-vars e))
         (missing (if (eq assigned :all) '()
                      (remove-if (lambda (v) (member v assigned :test #'name=)) must-assign))))
    (cond ((and (null missing) (not (typep e 'all-expr))) (emit-action-item e stream must-assign))
          (t (emit-all (if (typep e 'all-expr) e (make-instance 'all-expr :forms (list e)))
                       stream must-assign :extra-frame missing)))))

(defun emit-all (e stream must-assign &key extra-frame)
  (let ((items (block-forms e)))
    (if (and (null items) (null extra-frame))
        (write-string "true" stream)
        (progn
          (write-string "all {" stream)
          (with-indent
            (dolist (f items)
              (%nl stream) (emit-action-item f stream must-assign) (write-string "," stream))
            (dolist (v extra-frame)
              (%nl stream) (format stream "~a' = ~a," (%decl-ref v) (%decl-ref v))))
          (%nl stream) (write-string "}" stream)))))

(defun emit-action-body (op stream)
  "requires + guards + body + frame for vars the action never touches."
  (let* ((all-vars (mapcar #'decl-lisp-name (module-vars *module*)))
         (assigned (assigned-vars (op-body op)))
         (conjuncts (append (op-requires op) (op-guards op)))
         (frame (if (eq assigned :all) '()
                    (remove-if (lambda (v) (member v assigned :test #'name=)) all-vars)))
         (must-assign (if (eq assigned :all) '() assigned)))
    (cond
      ((and (null conjuncts) (null frame))
       (emit-branch (op-body op) stream must-assign))
      (t
       ;; Splice a top-level (all …) body into the outer all instead of nesting blocks.
       (let ((items (if (typep (op-body op) 'all-expr) (block-forms (op-body op)) (list (op-body op)))))
         (write-string "all {" stream)
         (with-indent
           (dolist (c conjuncts) (%nl stream) (emit-expr c stream) (write-string "," stream))
           (dolist (f items) (%nl stream) (emit-action-item f stream must-assign) (write-string "," stream))
           (dolist (v frame) (%nl stream) (format stream "~a' = ~a," (%decl-ref v) (%decl-ref v))))
         (%nl stream) (write-string "}" stream))))))

;;; Declarations

(defun %return-type (op)
  "Quint requires a return type whenever parameters are annotated. Actions are bool;
defs without (:returns …) are emitted without parameter annotations."
  (or (op-return-type op)
      (and (eq (op-mode op) :action) (make-instance 'type-ref :name :bool))))

(defun %params-text (op)
  (let ((annotate (%return-type op)))
    (with-output-to-string (s)
      (loop for ((p . ty) . rest) on (op-params op)
            do (write-string (%name p) s)
               (when (and ty annotate) (write-string ": " s) (emit-type ty s))
               (when rest (write-string ", " s))))))

(defun emit-op (op stream)
  (when (decl-doc op) (%nl stream) (format stream "/// ~a" (decl-doc op)))
  (%nl stream)
  (let ((keyword (ecase (op-mode op)
                   (:pure-val "pure val") (:pure-def "pure def") (:val "val") (:def "def")
                   (:action "action") (:temporal "temporal") (:run "run"))))
    (format stream "~a ~a" keyword (decl-name op))
    (when (or (op-params op) (member (op-mode op) '(:pure-def :def)))
      (format stream "(~a)" (%params-text op)))
    (let ((rt (%return-type op)))
      (when (and rt (or (op-params op) (op-return-type op)))
        (write-string ": " stream) (emit-type rt stream)))
    (write-string " = " stream)
    (if (eq (op-mode op) :action)
        (emit-action-body op stream)
        (emit-expr (op-body op) stream))))

(defun emit-decl (d stream)
  (etypecase d
    (const-decl (%nl stream) (format stream "const ~a: " (decl-name d)) (emit-type (decl-type d) stream))
    (var-decl (%nl stream) (format stream "var ~a: " (decl-name d)) (emit-type (decl-type d) stream))
    (type-decl (%nl stream) (format stream "type ~a = " (decl-name d)) (emit-type (type-decl-type d) stream))
    (assume-decl (%nl stream) (format stream "assume ~a = " (decl-name d)) (emit-expr (op-body d) stream))
    (import-decl
     (%nl stream)
     (format stream "import ~a" (mangle-type-name (import-module d)))
     (when (import-overrides d)
       (write-string "(" stream)
       (loop for ((c . x) . rest) on (import-overrides d)
             do (format stream "~a = " (%name c)) (emit-expr x stream) (when rest (write-string ", " stream)))
       (write-string ")" stream))
     (cond ((import-alias d) (format stream " as ~a" (import-alias d)))
           ((import-wildcard-p d) (write-string ".*" stream))))
    (op-def (emit-op d stream))))

(defun emit-module (module stream)
  "Write MODULE (and its instance module, which hosts the runs) as Quint."
  (let* ((*module* module)
         (*indent* 0)
         (instance (module-instance module))
         (runs (module-runs module)))
    (format stream "module ~a {" (module-name module))
    (with-indent
      (dolist (d (module-decls module))
        (unless (and instance (member d runs))
          (emit-decl d stream))))
    (format stream "~%}~%")
    (when instance
      (format stream "~%module ~a {" (instance-name instance))
      (with-indent
        (%nl stream)
        (format stream "import ~a(" (module-name module))
        (loop for ((c . x) . rest) on (instance-bindings instance)
              do (format stream "~a = " (%decl-ref c)) (emit-expr x stream) (when rest (write-string ", " stream)))
        (write-string ").*" stream)
        (dolist (r runs) (emit-op r stream)))
      (format stream "~%}~%"))
    module))

(defun emit-to-string (module)
  (with-output-to-string (s) (emit-module module s)))
