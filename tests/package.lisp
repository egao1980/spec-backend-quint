(defpackage #:spec-backend-quint/tests
  (:use #:cl #:rove #:spec-protocol #:spec-backend-quint)
  (:shadowing-import-from #:rove #:run-tests))
(in-package #:spec-backend-quint/tests)

(defun fixture (name)
  (asdf:system-relative-pathname "spec-backend-quint" (format nil "tests/fixtures/~a" name)))

(defun fixture-text (name)
  "Read a fixture as UTF-8. Drop CR so a Windows checkout (CRLF) still matches LF emit."
  (with-open-file (in (fixture name) :external-format :utf-8)
    (let* ((text (make-string (file-length in)))
           (n (read-sequence text in)))
      (remove #\Return (subseq text 0 n)))))

(defmacro with-quint (&body body)
  "Run BODY only when the quint CLI is installed (QUINT_BIN or PATH); otherwise SKIP."
  `(if (quint-available-p)
       (progn ,@body)
       (skip "quint not installed")))

(defmacro with-java (&body body)
  `(if (and (quint-available-p) (java-available-p))
       (progn ,@body)
       (skip "JAVA_HOME (JDK >= 17) not set")))
