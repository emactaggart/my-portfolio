(defpackage control
  (:use :cl :hunchentoot)
  (:export #:main
           #:start-server
           #:stop-server))

(in-package control)

(defvar **acceptor** nil)

(defun main ()
  (print "Starting server on port 8080")

  ;; this should be in the thread?
  (start-server)
  ;; let the webserver run.
  ;; warning: hardcoded "hunchentoot".
  (handler-case (bt:join-thread (find-if (lambda (th)
                                           (search "hunchentoot" (bt:thread-name th)))
                                         (bt:all-threads)))
    ;; Catch a user's C-c
    (#+sbcl sb-sys:interactive-interrupt
     #+ccl  ccl:interrupt-signal-condition
     #+clisp system::simple-interrupt-condition
     #+ecl ext:interactive-interrupt
     #+allegro excl:interrupt-signal
     () (progn
          (format *error-output* "Aborting.~&")
          (stop-server)
          (uiop:quit)))
    (error (c) (format t "Woops, an unknown error occured:~&~a~&" c))))

(defun start-server ()
  (setf **acceptor**
        (start (make-instance 'easy-acceptor
                              :port 8080
                              :document-root #p"/dev/null"
                              ;; :acceptor-error-template-directory #p"/dev/null"
                              :access-log-destination (merge-pathnames "access.log")
                              :message-log-destination (merge-pathnames "message.log")
                              ))))

(defun stop-server ()
  (when (not (hunchentoot::acceptor-shutdown-p **acceptor**))
    (stop **acceptor**)))

