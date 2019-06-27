;; NOTE: maybe use this for deployment of webserver

(require 'sb-daemon)
(require 'swank)
(require 'mysite)

(progn
  (defparameter *running* nil)
  (defun launch-app (argv)
    (declare (ignore argv))
    (sb-daemon:daemonize :output "/tmp/mysite.output"
                         :error "/tmp/mysite.error"
                         :pidfile "/tmp/mysite.pid"
                         :exit-parent t
                         :sigterm (lambda (sig)
                                    (declare (ignore sig))
                                    (setf *running* nil)))
    (swank:create-server :port 4006 :dont-close t)
    (setf *running* t)
    (mysite:start-app)
    (loop while *running* do (sleep 10))
    (mysite:stop-app)
    (sb-ext:exit))
  (launch-app nil))
