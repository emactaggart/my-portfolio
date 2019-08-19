(defpackage control
  (:use :cl :hunchentoot)
  (:export #:main
           #:start-server
           #:stop-server))

(in-package control)

(defvar **acceptor** nil)

(if (string= "DEV" (config:get-config "PROFILE"))
    (progn (setf hunchentoot:*show-lisp-errors-p* t)
           (setf hunchentoot:*show-lisp-backtraces-p* t)
           (setf hunchentoot:*log-lisp-warnings-p* t)
           (setf hunchentoot:*log-lisp-errors-p* t)
           (setf hunchentoot:*log-lisp-backtraces-p* t)
           (push (create-prefix-dispatcher "/test" 'test-handler) *dispatch-table*))
    (progn (setf hunchentoot:*show-lisp-errors-p* nil)
           (setf hunchentoot:*show-lisp-backtraces-p* nil)
           (setf hunchentoot:*log-lisp-warnings-p* t)
           (setf hunchentoot:*log-lisp-errors-p* nil)
           (setf hunchentoot:*log-lisp-backtraces-p* nil)
           (setf hunchentoot:*catch-errors-p* t)))

;; FIXME do we even need this?
(defun main ()
  (print "starting server on port 8080")

  ;; this should be in the thread?
  (start-server)
  ;; let the webserver run.
  ;; warning: hardcoded "hunchentoot".
  (handler-case (bt:join-thread (find-if (lambda (th)
                                           (search "hunchentoot" (bt:thread-name th)))
                                         (bt:all-threads)))
    ;; catch a user's c-c
    (#+sbcl sb-sys:interactive-interrupt
     #+ccl  ccl:interrupt-signal-condition
     #+clisp system::simple-interrupt-condition
     #+ecl ext:interactive-interrupt
     #+allegro excl:interrupt-signal
     () (progn
          (format *error-output* "aborting.~&")
          (stop-server)
          (uiop:quit)))
    (error (c) (format t "woops, an unknown error occured:~&~a~&" c))))

(defun start-server ()
  (setf **acceptor**
        (start (make-instance 'easy-acceptor
                              :port 8080
                              ;; :document-root #p"/dev/null" ;; TODO eventually create custom error pages
                              ;; :acceptor-error-template-directory #p"/dev/null"
                              :access-log-destination (merge-pathnames "access.log")
                              :message-log-destination (merge-pathnames "message.log")))))

(defun stop-server ()
  (when (not (hunchentoot::acceptor-shutdown-p **acceptor**))
    (stop **acceptor**)))

