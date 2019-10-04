(defpackage :config
  (:use :cl)
  (:export :get-config :get-config-or-error :load-configs))

(in-package :config)

(defvar configs (make-hash-table :test 'equal))

(defun load-configs (config-file)
  (when (not (uiop:file-exists-p config-file))
    (error "~a" (concatenate 'string "Cannot find configuration file: " config-file)))
  (with-open-file (stream config-file)
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
