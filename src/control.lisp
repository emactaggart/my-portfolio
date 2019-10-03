(defpackage :control
  (:use :cl :hunchentoot :bordeaux-threads)
  (:export #:main
           #:start-server
           #:stop-server))

(in-package control)

(defvar **acceptor** nil)
(defvar log-directory (truename (merge-pathnames config:*application-name* #p"/var/log/")))

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

(defun start-server ()
  (setf **acceptor**
        (start (make-instance 'easy-acceptor
                              :port 8080
                              ;; :document-root #p"/dev/null" ;; TODO eventually create custom error pages
                              ;; :acceptor-error-template-directory #p"/dev/null"
                              :access-log-destination (merge-pathnames "access.log" log-directory)
                              :message-log-destination (merge-pathnames "message.log" log-directory))))


  (handler-case (bt:join-thread (find-if (lambda (th)
                                           (search "hunchentoot" (bt:thread-name th)))
                                         (bt:all-threads)))
    (#+sbcl sb-sys:interactive-interrupt
     () (progn
          (format *error-output* "Aborting.~&")
          (stop-server)
          (uiop:quit)))
    (error (c) (format t "Woops, an unknown error occured:~&~a~&" c))))

(defun stop-server ()
  (when (not (hunchentoot::acceptor-shutdown-p **acceptor**))
    (stop **acceptor**)))



;; FIXME put this somewhere else...
(when (string= (config:get-config "PROFILE") "DEV")
  (defun test-handler ()
    (who:with-html-output (*standard-output* nil :prologue t :indent nil)
      (:html
       (:body
        (:div "hello world")))))
  (defun always-success-handler ()
    nil)
  (defun always-error-handler ()
    (setf (hunchentoot:return-code*) 400)
    nil)
  (push (create-regex-dispatcher "^/test$" 'test-handler) *dispatch-table*)
  (push (create-regex-dispatcher "^/success$" 'always-success-handler) *dispatch-table*)
  (push (create-regex-dispatcher "^/error$" 'always-error-handler) *dispatch-table*)
  )

