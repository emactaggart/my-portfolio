(defpackage :my-portfolio-tests
  (:use :cl :fiveam)
  (:export :run-all-tests-and-fail))

(define-condition tests-failed (error) ())

(in-package :my-portfolio-tests)

;; TODO turn this into something similar to labels
(defmacro with-mock-function (fdef &rest body)
  (let ((oldf (gensym))
        (result (gensym))
        (name (car fdef))
        (args (cadr fdef))
        (rbody (cddr fdef)))
    `(let ((,oldf (symbol-function ',name)))
       (setf (symbol-function ',name) (lambda ,args ,@rbody))
       (let ((,result (progn ,@body)))
         (setf (symbol-function ',name) ,oldf)
         ,result))))

(defun run-all-tests-and-fail ()
  (unless (run-all-tests)
    (error 'tests-failed :value "Tests Failed")))

(def-suite portfolio-tests
  :description "The master suite of all tests.")

(in-suite portfolio-tests)

(test dummy-tests
  "A Placeholder"
  (is (listp (list 1 2)))
  (is (= 5 (+ 2 3))))


(def-suite mailgun-client-tests
  :description "Suite of mailgun-client tests"
  :in portfolio-tests)

(in-suite mailgun-client-tests)

(def-fixture no-api-key ()
  (let ((mailgun-client::*api-key* nil))
    (&body)))

(def-fixture some-api-key ()
  (let ((mailgun-client::*api-key* t))
    (&body)))

(test send-message-no-api-key
  :description "send-message without *api-key* populated"
  (with-fixture no-api-key ()
    (signals simple-error (mailgun-client::send-message "to" "from" "subject" "message"))))

(test send-message-has-api-key
  :description "send-message, having *api-key* populated"
  (with-fixture some-api-key ()
    (signals simple-error (mailgun-client::send-message "" "" "" ""))

    (with-mock-function (dex:post (&rest rest) t)
      (finishes (mailgun-client::send-message "to" "from" "subject" "message")))

    (with-mock-function
        (dex:post (&rest rest)
                  (error "Some Bad Request"))
      (with-mock-function (hunchentoot:log-message* (&rest rest) t)
        (signals simple-error (mailgun-client::send-message "to" "from" "subject" "message"))))))


(def-suite handler-tests
  :description "Portfolio handler tests"
  :in portfolio-tests)

(in-suite handler-tests)

(test email-address-p-tests
  (is-true (handler::email-address-p "abc@123.ca"))
  (is-false (handler::email-address-p "abc123ca")))

(test validate-tests
  (let ((example-validations `((,(lambda (name) (> (length name) 0)) "Please enter a name")
                               (,(lambda (name) (< (length name) 10)) ,(format nil "Name must be less than ~a characters." 10)))))
    (is (equal (handler::validate "name" "John Doe" example-validations)
               '("name" nil)))
    (is (equal (handler::validate "name" "" example-validations)
               '("name" ("Please enter a name"))))
    (is (equal (handler::validate "name" "I have a really really long name" example-validations)
               '("name" ("Name must be less than 10 characters."))))))

(test validate-all-tests
  (let ((inputs '(:name "John Doe"
                  :email "john.doe@gmail.ca"
                  :message "Hello world"))
        (validations handler::*message-handler-validations*))
    (is (equal (handler::validate-all inputs validations)
               nil)))
  (let ((inputs '(:name ""
                  :email "john."
                  :message ""))
        (validations handler::*message-handler-validations*))
    (is (equal (handler::validate-all inputs validations)
               '(:name ("Please enter a name")
                 :email ("Invalid email address.")
                 :message ("Please enter a message."))))))


;; How should we do this? Verify status code? Verify some content?
;; TODO message-handler
;; TODO http code handler 404 and 500
;; TODO profile-handler
;; TODO static content

