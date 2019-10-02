(defpackage :my-portfolio-tests
  (:use :cl :fiveam))

(in-package :my-portfolio-tests)

(def-suite all-tests
  :description "Thpe master suite of all tests.")

(in-suite all-tests)

(test dummy-tests
  "A Placeholder"
  (is (listp (list 1 2)))
  (is (= 5 (+ 2 3))))

