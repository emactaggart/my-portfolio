(defpackage control
  (:use :cl :hunchentoot)
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
                              :message-log-destination (merge-pathnames "message.log" log-directory)))))

(defun stop-server ()
  (when (not (hunchentoot::acceptor-shutdown-p **acceptor**))
    (stop **acceptor**)))

