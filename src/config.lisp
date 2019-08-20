(defpackage #:config
  (:use #:cl)
  (:export #:get-config #:get-config-or-error #:*application-root*))

(in-package #:config)

(defvar configuration-file-pathname (truename #p"~/.devrc"))
(defvar configs (make-hash-table :test 'equal))

(defun load-configs ()
  (when (not (uiop:file-exists-p configuration-file-pathname))
    (error "~a" (concatenate 'string "Cannot find configuration file: " configuration-file-pathname)))
  (with-open-file (stream configuration-file-pathname)
    (loop for line = (read-line stream nil)
          while line
          do
             (destructuring-bind (key value) (split-sequence:split-sequence #\= line)
               (setf (gethash key configs) (string-trim " " value))))))

(defun get-config (config-name)
  (gethash config-name configs))

(defun get-config-or-error (config-name)
  (let ((config (get-config config-name)))
    (if config
        config
        (error (format nil "Could not find configuration ~s" config-name)))))

(load-configs)
(defvar *application-root* (truename (get-config-or-error "APPLICATION_ROOT")))
