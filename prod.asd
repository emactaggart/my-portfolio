(defsystem "prod"
  :version "0.0.0"
  :author ""
  :license ""
  :depends-on (:alexandria
               :bordeaux-threads
               :cl-arrows
               :cl-fad
               :cl-ppcre
               :cl-who
               :dexador
               :hunchentoot
               :jsown
               ;; :log4cl ;; implement this later and replace hunchentoot's default logging
               :parenscript
               :prove
               :uiop)
  :components ((:module "src"
                :components
                ((:file "config")
                 (:file "email-sender"
                  :depends-on ("config"))
                 (:file "handler"
                  :depends-on ("email-sender"))
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
