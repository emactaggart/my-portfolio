(defpackage :my-portfolio-tests
  (:use :cl :fiveam)
  (:export :run-all-tests-and-fail))

(define-condition tests-failed (error) ())

(in-package :my-portfolio-tests)

(def-suite all-tests
  :description "Thpe master suite of all tests.")

(in-suite all-tests)

(test dummy-tests
  "A Placeholder"
  (is (listp (list 1 2)))
      (is (= 5 (+ 2 3))))

(defun run-all-tests-and-fail ()
  (unless (run-all-tests)
    (error 'tests-failed :value "Tests Failed")))


