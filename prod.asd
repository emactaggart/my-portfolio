(defsystem "prod"
  :version "0.0.0"
  :author ""
  :license ""
  :depends-on (:cl-fad
               :cl-who
               :hunchentoot
               :parenscript
               :prove
               :bordeaux-threads)
  :components ((:module "src"
                :components
                ((:file "handler")
                 (:file "control"
                  :depends-on ("handler")))))
  :description ""
  :in-order-to ((test-op (test-op "prod/tests"))))

(defsystem "prod/tests"
  :author ""
  :license ""
  :depends-on ("prod"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for prod"
  :perform (test-op (op c) (symbol-call :rove :run c)))
