(in-package #:spec-backend-quint/tests)

(deftest bank-golden
  (ok (string= (fixture-text "bank.qnt") (emit-to-string (find-spec 'spec-protocol.examples:bank)))
      "emitted Quint matches tests/fixtures/bank.qnt (regenerate with (emit …), never by hand)"))

(defspec frame (:module "Frame")
  (var x int)
  (var y int)
  (var z int)
  (action init (setf x 0 y 0 z 0))
  (action only-x (incf x))
  (action branchy (if (> x 0) (setf x 0) (setf y 1)))
  (action matchy (match (if (> x 0) :pos :zero) (:pos (setf x 0)) (_ (setf y 1 z 1))))
  (action guarded (:requires (> y 0)) (:guard (< x 10)) (decf y)))

(defun emitted (name)
  (let ((text (emit-to-string (find-spec 'frame))))
    (let* ((start (search (format nil "action ~a" name) text))
           (end (search (format nil "~%  action") text :start2 (1+ start))))
      (subseq text start (or end (length text))))))

(deftest frame-conditions-for-untouched-vars
  (let ((text (emitted "onlyX")))
    (ok (search "x' = (x + 1)" text))
    (ok (search "y' = y" text))
    (ok (search "z' = z" text))))

(deftest frame-conditions-per-branch
  (let ((text (emitted "branchy")))
    ;; both branches must assign x and y; z is framed once at the action level
    (ok (search "if ((x > 0)) all {" text))
    (ok (= 1 (count-matches "y' = y" text)) "then-branch frames y")
    (ok (= 1 (count-matches "x' = x," text)) "else-branch frames x")
    (ok (= 1 (count-matches "z' = z" text)))))

(deftest frame-conditions-in-match
  (let ((text (emitted "matchy")))
    (ok (search "| Pos => all {" text))
    (ok (search "| _ => all {" text))
    (ok (search "y' = y," text))
    (ok (search "z' = z," text))
    (ok (search "x' = x," text))))

(deftest requires-and-guards-are-conjuncts
  (let ((text (emitted "guarded")))
    (ok (search "(y > 0)," text))
    (ok (search "(x < 10)," text))
    (ok (search "y' = (y - 1)" text))))

(defun count-matches (needle haystack)
  (loop with start = 0 for pos = (search needle haystack :start2 start)
        while pos count t do (setf start (1+ pos))))

(deftest instance-module-hosts-runs
  (let ((text (emit-to-string (find-spec 'spec-protocol.examples:bank))))
    (ok (search (format nil "module BankTest {~%  import Bank(accounts = Set(\"a\", \"b\")).*~%  run happyTest") text))
    (ok (not (search "run happyTest" text :end2 (search "module BankTest" text))) "runs are not in the main module")))

(deftest annotated-params-need-return-type
  (let ((text (emit-to-string (find-spec 'spec-protocol.examples:bank))))
    (ok (search "pure def canWithdraw(b: int, n: int): bool =" text))
    (ok (search "action deposit(who: str, n: int): bool =" text))))
