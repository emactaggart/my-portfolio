(defpackage :mailgun-client
  (:use :cl)
  (:export :send-to-self))

(in-package :mailgun-client)

(defvar *api-key* (config:get-config "MAILGUN_API_KEY"))
(defvar *domain* "sandbox61d987eb88cd452eace1c7584ca34ec0.mailgun.org")
(defvar *mailgun-messages-endpoint* (concatenate 'string "https://api.mailgun.net/v3/" *domain* "/messages"))

(defun send-message (to from subject message)
  (when (not *api-key*)
    (error "No api key, cannot send emails"))
  (when (or (null to)
            (null from)
            (null subject)
            (null message))
    (error "Incomplete email information"))

  (handler-case (dex:post *mailgun-messages-endpoint*
                    :basic-auth `("api" . ,*api-key*)
                    :content `(("from" . ,from)
                               ("to" . ,to)
                               ("subject" . ,subject)
                               ("text" . ,message))
                    :verbose t)
    (t (e)
      (log-message* :error
       "Failed to send email with parameters from: ~s to: ~s subject: ~s text: ~s" from to subject message)
       e )))

(defun send-to-self (name from message)
  (let ((from (concatenate 'string name " <" from ">"))
        (to "evan.mactaggart@gmail.com")
        (subject (concatenate 'string "Inquiry from " name))
        (message message))
    (send-message to from subject message)))
