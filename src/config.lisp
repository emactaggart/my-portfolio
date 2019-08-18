(defpackage #:config
  (:use #:cl)
  (:export #:get-config))

(in-package #:config)

;; FIXME encrypt api keys
(defvar **secrets-location** "~/.lisp-app-secrets-rc")
(defvar **configs** (make-hash-table :test 'equal))

(defun load-configs ()
  (when (not (uiop:file-exists-p **secrets-location**))
    (error "~a" (concatenate 'string "Cannot find configuration file: " **secrets-location**)))
  (with-open-file (stream **secrets-location**)
    (loop for line = (read-line stream nil)
          while line
          do
             (destructuring-bind (key value) (split-sequence:split-sequence #\= line)
               (setf (gethash key **configs**) (string-trim " " value))))))

(defun get-config (config-name)
  (gethash config-name **configs**))

(load-configs)

