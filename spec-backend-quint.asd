(defsystem "spec-backend-quint"
  :version "0.1.0"
  :description "Quint backend for spec-protocol: emit .qnt, drive quint typecheck/run/test/verify/compile"
  :author "egao1980"
  :license "MIT"
  :depends-on ("spec-protocol" "process-protocol" "json-protocol" (:version "cl-stack-pathlib" "0.3.0") "babel")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "emit")
               (:file "backend"))
  :in-order-to ((test-op (test-op "spec-backend-quint/tests"))))

(defsystem "spec-backend-quint/tests"
  :depends-on ("spec-backend-quint" "process-backend-uiop" "json-backend-jzon" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "emit-test")
               (:file "quint-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
