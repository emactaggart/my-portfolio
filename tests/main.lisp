(defpackage prod/tests/main
  (:use :cl
        :prod
        :rove))
(in-package :prod/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :prod)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
