(defpackage my-portfolio/tests/main
  (:use :cl
        :my-portfolio
        :rove))
(in-package :my-portfolio/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :my-portfolio)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
