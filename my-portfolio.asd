(defsystem "my-portfolio"
  :version "0.0.0"
  :author ""
  :license ""
  :build-operation "program-op"
  :build-pathname "dist/my-portfolio.exe"
  :entry-point "control:start-blocking-server"
  :depends-on ("alexandria"
               "bordeaux-threads"
               "arrow-macros"
               "cl-ppcre"
               "cl-who"
               "str"
               "dexador"
               "hunchentoot"
               "jsown"
               ;; "log4cl" ;; implement this later and replace hunchentoot's default logging
               "parenscript"
               "prove"
               "uiop")
  :components ((:module "src"
                :serial t
                :components
                ((:file "config")
                 (:file "mailgun-client")
                 (:file "handler")
                 (:file "control"))))
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
                    (symbol-call :my-portfolio-tests :run-all-tests-and-fail)))

#+sb-core-compression
(defmethod asdf:perform ((op asdf:image-op) (system asdf:system))
  (uiop:dump-image (asdf:output-file op system) :executable t :compression t))
