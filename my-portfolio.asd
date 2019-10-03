(defsystem "my-portfolio"
  :version "0.0.0"
  :author ""
  :license ""
  :build-operation "program-op"
  :build-pathname "my-portfolio.exe"
  :entry-point "control:start-server"
  :depends-on ("alexandria"
               "bordeaux-threads"
               "cl-arrows"
               "cl-ppcre"
               "cl-who"
               "dexador"
               "hunchentoot"
               "jsown"
               ;; "log4cl" ;; implement this later and replace hunchentoot's default logging
               "parenscript"
               "prove"
               "uiop")
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
  :depends-on (:my-portfolio
               :fiveam)
  :components ((:module "tests"
                :serial t
                :components
                ((:file "main"))))
  :components ((:file "main"))
  :description "Test system for my-portfolio"
  :perform (test-op (op system)
                    (symbol-call :fiveam :run-all-tests)))

#+sb-core-compression
(defmethod asdf:perform ((op asdf:image-op) (system asdf:system))
  (uiop:dump-image (asdf:output-file op system) :executable t :compression t))
