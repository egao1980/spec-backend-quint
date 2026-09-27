(in-package #:spec-backend-quint/tests)

(setup
  (process-backend-uiop:use-uiop-process-backend)
  (json-backend-jzon:use-jzon-backend)
  (use-quint-backend))

(deftest backend-is-installed
  (ok (typep *spec-backend* 'quint-backend)))

(deftest missing-tool-signals
  (ok (signals (typecheck 'spec-protocol.examples:bank :backend (make-quint-backend :program "/nonexistent/quint"))
               'spec-tool-missing-error)))

(deftest typecheck-bank
  (with-quint
    (let ((r (typecheck 'spec-protocol.examples:bank)))
      (ok (eq :ok (result-status r)))
      (ok (null (result-errors r))))))

(defspec ill-typed (:module "IllTyped")
  (var x int)
  (action init (setf x 0))
  (action bad (setf x (:lisp "not emitted"))))

(defspec ill-typed-quint (:module "IllTypedQuint" :clos nil)
  (var x int)
  (action init (setf x 0))
  (:spec-only (action bad (setf x (:raw "\"s\"")))))

(deftest typecheck-reports-errors
  (with-quint
    (let ((r (typecheck 'ill-typed-quint)))
      (ok (eq :error (result-status r)))
      (ok (some (lambda (e) (search "QNT000" e)) (result-errors r))))))

(deftest run-tests-bank
  (with-quint
    (let ((r (spec-protocol:run-tests 'spec-protocol.examples:bank)))
      (ok (eq :ok (result-status r)))
      (ok (equal '("happyTest") (result-passed r))))))

(deftest simulate-and-replay
  (with-quint
    (let ((r (simulate 'spec-protocol.examples:bank :seed 11 :n-traces 2 :max-steps 6)))
      (ok (eq :ok (result-status r)))
      (ok (= 2 (length (result-traces r))))
      (dolist (tr (result-traces r))
        (ok (typep (spec-protocol.mbt:replay-trace (spec-protocol.mbt:make-clos-driver 'spec-protocol.examples:bank) tr)
                   'spec-protocol.examples:bank))))))

(deftest simulate-finds-violation
  (with-quint
    ;; an ad-hoc invariant expression that the model can break: a never exceeds 2
    (let ((r (simulate 'spec-protocol.examples:bank :invariants '("balances.get(\"a\") <= 2")
                                                    :seed 3 :max-steps 8 :max-samples 50)))
      (ok (eq :violation (result-status r)))
      (ok (result-seed r))
      (ok (= 1 (length (result-traces r))))
      (ok (signals (check 'spec-protocol.examples:bank :invariants '("balances.get(\"a\") <= 2") :seed 3 :max-steps 8)
                   'spec-invariant-violation)))))

(deftest compile-to-tlaplus
  (with-java
    (let ((r (compile-spec 'spec-protocol.examples:bank :target :tlaplus)))
      (ok (eq :ok (result-status r)))
      (ok (uiop:string-prefix-p "---- MODULE BankTest" (result-output r))))))

(deftest verify-with-apalache
  (with-java
    (let ((r (verify 'spec-protocol.examples:bank :max-steps 3)))
      (ok (eq :ok (result-status r)))
      (ok (eq :apalache (result-tool r))))))
