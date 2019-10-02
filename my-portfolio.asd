(defsystem "my-portfolio"
  :version "0.0.0"
  :author ""
  :license ""
  :depends-on (:alexandria
               :bordeaux-threads
               :cl-arrows
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
                 (:file "mailgun-client"
                  :depends-on ("config"))
                 (:file "handler"
                  :depends-on ("mailgun-client"))
                 (:file "control"
                  :depends-on ("config" "handler")))))
  :description ""
  :in-order-to ((test-op (test-op "my-portfolio/tests"))))

(defsystem "my-portfolio/tests"
  :author ""
  :license ""
  :depends-on ("my-portfolio"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for my-portfolio"
  :perform (test-op (op c) (symbol-call :rove :run c)))
