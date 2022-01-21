
(defpackage :control
  (:use :cl :hunchentoot :bordeaux-threads :config)
  (:export :main
           :start-server
           :start-dev-server
           :stop-server)
  (:import-from :handler :configure-handlers :http-code-handler)
  (:import-from :mailgun-client :configure-mail-client))

(in-package control)

;; Defining our own acceptor to override the http-code error handling
(defclass portfolio-acceptor (easy-acceptor) ())

;; TODO dispatch on a range of status codes (and (>= 400 n) (< 500 n)) hunchentoot:acceptor-dispatch-request hunchentoot:handle-request
;; or use static files as opposted to custom dynamic handlers
(defmethod hunchentoot:acceptor-status-message
    ((acceptor portfolio-acceptor) (http-status-code (eql 404)) &key)
  (http-code-handler 404 "Not found."))

(defmethod hunchentoot:acceptor-status-message
    ((acceptor portfolio-acceptor) (http-status-code (eql 500)) &key)
  (http-code-handler 500 "Something appears to have exploded..."))

(defclass secure-reply (reply)
  ((headers-out
    ;; Preventing Hunchentoot server version leak... This could also be done via nginx server
    :initform '((:server . "Nginx")))))

;; FIXME should the configuration start here? perhaps pull out into a separate function
;; FIXME due to the async the repl now freezes when the app is started
;; FIXME killing seems not as simple...
;; TODO Killing this while running jams up the port; release port
(defun start-server-with-config (config-path)
  (let ((config-file (truename config-path)))
    (load-configs config-file)
    (let* ((profile (get-config "PROFILE"))
           (application-root (truename (get-config-or-error "APPLICATION_ROOT")))
           (application-name (get-config-or-error "APPLICATION_NAME"))
           ;; (log-directory (truename (merge-pathnames application-name #p"/var/log/"))) ;; NOTE: we now print out to stdout/err and GCP logging handles the storage
           (api-key (get-config-or-error "MAILGUN_API_KEY")))
      (configure-hunchentoot profile)
      (configure-test-handlers profile)
      (configure-handlers application-root)
      (configure-mail-client api-key)
      (start (make-instance 'portfolio-acceptor
                                   :port (or (parse-integer (get-config "PORT")) 8080)
                                   :reply-class 'secure-reply
                                   :access-log-destination *standard-output* ;; We are now logging to stdout where logs are magically gathered by a log aggregator
                                   :message-log-destination *standard-output*))
      (handler-case (bt:join-thread (find-if (lambda (th)
                                               (search "hunchentoot" (bt:thread-name th)))
                                             (bt:all-threads)))
        (#+sbcl sb-sys:interactive-interrupt
         () (progn
              (format *error-output* "Aborting.~&")
              (stop-server)
              (uiop:quit)))
        (error (c) (format t "Woops, an unknown error occured:~&~a~&" c)))
      )))

(defun start-dev-server ()
  (start-server-with-config "~/.taggrc"))

(defun start-server ()
  (start-server-with-config #p"/root/.taggrc"))

(defun stop-server ()
  (when (not (acceptor-shutdown-p *acceptor*))
    (stop *acceptor*)))

;; FIXME put this somewhere else?...
(defun configure-test-handlers (profile)
  (when (string= profile "DEV")
    (defun test-handler ()
      (error "wow")
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
    (push (create-regex-dispatcher "^/error$" 'always-error-handler) *dispatch-table*)))

(defun configure-hunchentoot (profile)
  (if (string= profile "DEV")
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
             (setf hunchentoot:*catch-errors-p* t))))
