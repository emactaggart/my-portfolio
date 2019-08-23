(defpackage #:handler
  (:use #:cl #:hunchentoot #:parenscript #:cl-fad #:cl-who))



(in-package #:handler)

(setf cl-who:*attribute-quote-char* #\")

;; TODO look further into ps namespacing
;; (setf (ps-package-prefix "HANDLER") "my_library_")

(defun log-handler-wrapper (fn &key (log-response-body nil))
  (lambda (&rest args)
    (log-message* :info "Calling function: ~a with args: ~s" fn args)
    (handler-case (let* ((result (multiple-value-list (apply fn args)))
                         (result-no-body (cdr result)))
                    (if log-response-body
                        (log-message* :info result)
                        (log-message* :info "Result: ~s" result-no-body))
                    (values-list result))
      (t (c)
        (log-message* :error "Something blew up when calling: ~a with ~s" fn args)
        c))))

(push (create-folder-dispatcher-and-handler "/css/" (merge-pathnames "css/" config:*application-root*)) *dispatch-table*)
(push (create-folder-dispatcher-and-handler "/webfonts/" (merge-pathnames "vendor/fontawesome-free-5.9.0-web/webfonts/" config:*application-root*)) *dispatch-table*)
(push (create-folder-dispatcher-and-handler "/resources/" (merge-pathnames "resources/" config:*application-root*)) *dispatch-table*)
(push (create-static-file-dispatcher-and-handler "/favicon.ico" (merge-pathnames "resources/favicon.ico" config:*application-root*)) *dispatch-table*)

(push (create-regex-dispatcher "^/$" (log-handler-wrapper 'profile-handler)) *dispatch-table*)
(push (create-regex-dispatcher "/send-message" (log-handler-wrapper 'message-handler :log-response-body t)) *dispatch-table*)

(defvar *email-address-regex* "\\A([\\w+\\-].?)+@[a-z\\d\\-]+(\\.[a-z]+)*\\.[a-z]+\\z")
(defvar *name-length* 50)
(defvar *message-length* 280)

(defparameter *message-handler-validations*
  `(:name ((,(lambda (name) (> (length name) 0)) "Please enter a name")
           (,(lambda (name) (< (length name) *name-length*)) (format nil "Name must be less than ~a characters." *name-length*)))
    :email ((,(lambda (email) (and (< (length email) 1024)
                                   (email-address-p email)))
             "Invalid email address."))
    :message ((,(lambda (name) (> (length name) 0)) "Please enter a message.")
              (,(lambda (name) (< (length name) *message-length*)) (format nil "Message must be less than ~a characters." *message-length*)))))

(defun message-handler ()
  (flet ((get-or-post-parameter (parameter-name)
           (or (post-parameter parameter-name) (get-parameter parameter-name) "")))
    (let* ((name (get-or-post-parameter "name"))
           (email (get-or-post-parameter "email"))
           (message (get-or-post-parameter "message"))
           (error-messages
             (validate-all `(:name ,name :email ,email :message ,message) *message-handler-validations*)))
      (if (alexandria:emptyp error-messages)
          (progn
            (email-sender:send-to-self name email message)
            (jsown:to-json '(:obj (:message . "good job"))))
          (progn
            (log-message* :warn "Bad inputs: " error-messages "~%")
            (setf (return-code*) 400)
            (jsown:to-json (alexandria:plist-hash-table error-messages)))))))

(defun validate-all (input-list validation-list)
  (let* ((error-messages (loop for (input-name input-value) on input-list :by #'cddr
                              collect
                              (let ((input-validations (getf validation-list input-name)))
                                (validate input-name input-value input-validations))))
        (non-nil-messages (remove-if #'null error-messages :key #'second)))
    (reduce #'concat-list non-nil-messages)))

(defun validate (input-name input-value input-validations)
  (let ((error-messages (loop for (fn err-message) in input-validations
                              collect
                              (when (not (funcall fn input-value))
                                err-message))))
    (list input-name (remove nil error-messages))))

(defun email-address-p (email-address)
  (= (length
      (cl-ppcre:all-matches-as-strings *email-address-regex* email-address))
     1))

(defun concat-list (&optional (a '()) (b '()))
  (concatenate 'list a b))

;; HANDLERS

(defmacro page-template ((&key title) &body body)
  `(with-html-output-to-string (*standard-output*
                                nil
                                :prologue t
                                :indent nil
                                )
     (:html
      (:head
       (:meta :charset "utf-8")
       (:meta :name "viewport" :content "width=device-width, initial-scale=1, shrink-to-fit=no")
       (:link :rel "stylesheet" :href "/css/bootstrap.min.css")
       (:link :rel "stylesheet" :href "/css/fontawesome-all.css")
       (:link :rel "icon" :href "favicon.ico")
       (:link :rel "stylesheet" :type "text/css" :href "css/main.css")
       (:title ,@title)
       (:script :src "https://code.jquery.com/jquery-3.2.1.min.js")
       )
      (:body :class "container-fluid w-100 p-0"
             ,@body

             (:script :src "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.12.9/umd/popper.min.js"
                      :integrity "sha384-ApNbgh9B+Y1QKtv3Rn7W3mgPxhU9K/ScQsAP7hUibX39j7fakFPskvXusvfa0b4Q"
                      :crossorigin "anonymous")
             (:script :src "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/js/bootstrap.min.js"
                      :integrity "sha384-JZR6Spejh4U02d8jOt6vLEHfe/JQGiRRSQQxSfFWpi1MquVdAyjUar5+76PVCmYl"
                      :crossorigin "anonymous")))))



(defun test-handler ()
  (page-template (:title "Test")
    (with-html-output (*standard-output* nil :prologue t :indent nil)
      (:html
       (:body
        (:div "hello world"))))))

(defun profile-handler ()
  (page-template (:title "Evan's Flavortown")
    (with-html-output (*standard-output*)
      (:script
       :type "text/javascript"
       (str
        (ps
          (defun smooth-scroll (location)
            (chain document
                   (query-selector location)
                   (scroll-into-view (create :behavior "smooth")))
            f))))

      (:section
       :id "home" :class "home d-flex flex-row"
       (:div :class "canvas-container h-100 w-100"
             :style "position:absolute; background: green;"
             (:canvas :class "crazy-canvas" "Please use a modern browser to view this site ):")
             ;; (:script
             ;;  :type "text/javascript"
             ;;  (str (canvas-ps))
             ;;  )
             )


       (:div :class "col align-self-center text-center"

             (:div :style "font-size: 2.5rem" "Hello, I'm "
              (:span :style "color: deeppink; font-weight: bold" "Evan MacTaggart."))

             (:div
              :style "font-size: 2.5rem"
              "A traveller and programmer.")
             ;; (:div :style "font-size: 2.5rem" (:b "(This definitely still a beta.)"))

             (:button :class "garbage-btn btn btn-primary btn-lg m-2 px-3 text-light"
                      :onclick
                      (str (ps
                             ((lambda ()
                                (chain document (query-selector "#about")
                                       (scroll-into-view (create :behavior "smooth")))))))

                      (:div :class "align-middle"
                            (:span :class "align-middle" :style "height: 100%;" "See more" )
                            (:i :class "fa fa-arrow-circle-right rotate-90-animation ml-2 align-middle")
                            ))))


      ;; FIXME clean up sticky-top and instead use use jquery? mobile chrome doesn't work very well...
      (:header
       :class "container-fluid sticky-top squeeze-out"
       (:div :class "d-flex flex-row py-1"
             (:a :class "nav-link offset-lg-2 offset-md-1" :href "#home" "HOME")
             (:a :class "nav-link" :href "#about" "ABOUT")
             (:a :class "nav-link" :href "#portfolio" "PORTFOLIO")
             (:a :class "nav-link" :href "#contact" "CONTACT")
             )

       (:script :type "text/javascript"
                (str (ps
                       (chain ($ window) (scroll (lambda ()
                                                   (if (>=
                                                        (chain ($ window) (scroll-top))
                                                        (- window.inner-height 200))
                                                       (chain ($ "header") (remove-class "squeeze-out"))
                                                       (chain ($ "header") (add-class "squeeze-out"))
                                                       ))))))))

      (:section :id "about" :class "about py-5"
                (:div :class "container text-center"

                      (:div :class "flex-row justify-content-center mb-3"
                            (:h1 (:b "ABOUT"))
                            (:div :class "d-flex justify-content-center"
                                  (:div :style "height: 5px; width: 120px; background: black;")))

                      (:div :class "row"

                            (:div :class "col-lg-3 col-md-6 col-xs-12"
                                  (:div
                                   (:div
                                    (:i :class "fa fa-5x fa-hamburger"))
                                   (:h3 "Full Stack"))
                                  (:p "From UX design to data modeling I'm stacked when it comes to full stack capabilities."))

                            (:div :class "col-lg-3 col-md-6 col-xs-12"
                                  (:div
                                   (:i :class "fa fa-5x fa-fighter-jet")
                                   (:h3 "Devops"))
                                  (:p "I'm not afraid to get my hands dirty on the command line and in build scripts."))

                            (:div :class "col-lg-3 col-md-6 col-xs-12"
                                  (:div
                                   (:i :class "fa fa-5x fa-hat-wizard")
                                   (:h3 "Imperfect"))
                                  (:p "I'm only human but always working towards attaining wizard status."))

                            (:div :class "col-lg-3 col-md-6 col-xs-12"
                                  (:div
                                   (:i :class "fa fa-5x fa-cogs")
                                   (:h3 "Simplicity"))
                                  (:p "Simplicity is the key to sane development. Likewise I prefer to avoid over-designing things.")))
                      (:div :style "border: 1px solid black;"
                            (:img :style "object-fit: contain; height: 300px; width: 300px;" :src "/resources/randy-marsh.png"))
                      (:div :class "py-3"
                            "I'm a simple developer capable of working full stack in a variety of technologies."
                            ;; I'm currently on vacation enjoying the simple life after having travelled South East Asia for the past 7 months.
                            "I'm currently an unemployed degenerate living in Regina Saskatchewan. I can do full stack.")

                      (let ((skills '((:name "JavaScript" :time "4 Years")
                                      (:name "Common Lisp" :time "3 Months")
                                      (:name "Linux" :time "5 Years")
                                      (:name "Docker" :time "6 Months")
                                      (:name "Angular" :time "2 Years")
                                      (:name "SQL" :time "2 Years")
                                      (:name "Python" :time "6 Months")
                                      (:name "C#" :time "8 Months")
                                      (:name "Closure" :time "2 Months")
                                      (:name "Java" :time "3 Years")
                                      (:name "Haskell" :time "6 Months")
                                      (:name "Vim" :time "2 Months")
                                      (:name "Emacs" :time "2 Months")
                                      )))
                        (loop for skill in skills
                              do
                                 (htm
                                  (:div :class "d-flex flex-row py-2"
                                        (:div :class "col-4 col-md-3 py-1"
                                              :style "background: salmon;"
                                              (str (getf skill :name)))
                                        (:div :class "col-8 col-md-9 py-1 d-flex align-items-center"
                                              :style "background: darksalmon;"
                                              (:div :class "ml-auto"
                                                    (str (getf skill :time))))
                                        )))
                        )
                      ))

      (:section :id "portfolio" :class "portfolio py-5 text-center"

                (:div :class "d-flex flex-row justify-content-center"
                      (:div
                       (:div :class "flex-row justify-content-center mb-3"
                             (:h1 (:b "WORK"))
                             (:div :class "d-flex justify-content-center"
                                   (:div :style "height: 5px; width: 120px; background: black;")))

                       (:p "Check me out on other platforms:")


                       (:div :class "row justify-content-center m-4"
                             (:div
                              (:div
                               (:a :class "m-2" :href "https://github.com/emactaggart"
                                   :style "color: black"
                                   (:i :class "fab fa-8x fa-github"))

                               (:a :class "m-2" :href "https://www.linkedin.com/in/evan-mactaggart-1a7826122"
                                   :style "color: black"
                                   (:i :class "fab fa-8x fa-linkedin"))

                               (:a :class "m-2 text-middle"
                                   :style "color: black"
                                   :href "https://drive.google.com/file/d/1sZk9o56LG1O-f8gmzVKcrvowXqAjBiCU/view"
                                   (:i :class "fab fa-8x fa-google-drive")))))
                       (:div :class "alert alert-warning"
                             (:i :class "fa fa-lg fa-hard-hat")
                             " My portfolio "
                             " is currently under construction. "
                             (:i :class "fa fa-lg fa-tools"))
                       )
                      ))

      (:section :id "contact" :class "contact py-5 h-100"

                (:div :class "h-100 container d-flex justify-content-center"
                      (:div :class "align-self-center col-12 col-sm-10 col-md-9 text-center"

                            (:h1 (:b "CONTACT"))
                            (:div :class "row justify-content-center"
                                  (:div :style "height: 5px; width: 120px; background: white;"))


                            (:div :class "py-3" :style "color: salmon"
                                  "Interested? Any questions? Shoot me a message!")

                            (:form :id "my-form" :action "/send-message" :method "post"

                                   (:div :class "input-group mb-1"
                                         (:input :id "name" :class "form-control"
                                                 :type "text" :name "name" :placeholder "Name"
                                                 :maxlength *name-length*
                                                 :required t
                                                 ))

                                   (:div :class "input-group mb-1"
                                         (:input :id "email" :class "form-control"
                                                 :type "email" :name "email" :placeholder "Email"
                                                 :required t
                                                 ))

                                   (:div :class "input-group mb-1"
                                         (:textarea :id "message"
                                                    :class "form-control"
                                                    :style "height: 150px"
                                                    :name "message" :placeholder "Your message."
                                                    :onkeypress (str (ps ((lambda (event)
                                                                            (chain ($ "#message-count")
                                                                                   (text (- 280
                                                                                            ;; FIXME getting lisp values in ps
                                                                                            ;; (getf *validation-values* :message-length)
                                                                                            (@ event target text-length)
                                                                                            1))))
                                                                          event)))

                                                    :maxlength *message-length*
                                                    :required t
                                                    )
                                         (:small
                                          :style "position: absolute; z-index: 5; right: 0.5em; bottom: 0.5em; color: grey;"
                                          "Characters remaining: "
                                          (:span :id "message-count" )
                                          (:script :type "text/javascript"
                                                   (str (ps
                                                          ((lambda ()
                                                             (chain
                                                              ($ document)
                                                              (ready
                                                               (lambda ()
                                                                 (let ((message-length (chain ($ "#message")
                                                                                              (val) length))))

                                                                 (chain ($ "#message-count")
                                                                        (text (-
                                                                               280
                                                                               ;; (lisp (getf *validation-values* :message-length))
                                                                               message-length
                                                                               )))
                                                                 )))
                                                             ))
                                                          ))))
                                         )

                                   (:button :class "btn garbage-btn mt-1 float-right"
                                            :type "submit"
                                            :id "submit"
                                            (:b :class "mx-3" "SUBMIT"))

                                   (:script :type "text/javascript"
                                            (str (ps
                                                   (chain ($ "#my-form")
                                                          (submit
                                                           (lambda (event)
                                                             (event.prevent-default)

                                                             (let* (($form ($ this))
                                                                    (url (chain $form (attr "action")))
                                                                    (form-data (create :name (chain ($ "#name") (val))
                                                                                       :email (chain ($ "#email") (val))
                                                                                       :message (chain ($ "#message") (val)))))
                                                               (chain $
                                                                      (post
                                                                       "send-message"
                                                                       form-data
                                                                       (lambda (data)
                                                                         (chain ($ "#name") (val ""))
                                                                         (chain ($ "#email") (val ""))
                                                                         (chain ($ "#message") (val ""))
                                                                         (alert "success")
                                                                         ))))

                                                             f)))

                                                   ))))
                            )))

      (:footer :class "footer py-5 position-relative"

               (:a :href "#home" :class "text-white"
                   (:div :class "text-center position-absolute"
                         :style "background: salmon; height: 50px; width: 50px; left: calc(50% - 25px); top: -25px;"
                         (:i :class "mt-2 fa fa-2x fa-upload")))


               (:div :class "d-flex flex-row justify-content-center py-3"
                     (:a :class "p-2 text-white" :href "https://github.com/emactaggart"
                         (:i :class "fab fa-3x fa-github-square p-2"))
                     (:a :class "p-2 text-white" :href "https://www.linkedin.com/in/evan-mactaggart-1a7826122"
                         (:i :class "fab fa-3x fa-linkedin p-2")))

               (:div :class "d-flex flex-row justify-content-center py-3"
                     (:small :style "color: grey" "EVAN MACTAGGART"
                             (:span :style "color: salmon"
                                    ;; (:i :class "fa fa-copyright")
                                    ;; (:i :class "fa fa-trademark")
                                    ;; (:i :class "fa fa-registered")
                                    " 2019"))))

      )))



(defun canvas-ps ()
  (ps

    (let ((canvas (chain document (query-selector "canvas"))))

      (setf canvas.width (chain ($ ".canvas-container") (width)))
      (setf canvas.height (chain ($ ".canvas-container") (height)))

      (defparameter c (chain canvas (get-context "2d")))
      (defvar max-radius 40)
      (defvar min-radius 2)
      (defparameter mouse (create :x undefined :y undefined))
      (defparameter color-array (make-array "#ffaa33" "#99ffaa" "#00ff00" "#4411aa" "#ff1100"))

      (window.add-event-listener
       "mousemove"
       (lambda (event)
         (setf mouse.x event.x)
         (setf mouse.y event.y)
         ))

      (window.add-event-listener
       "resize"
       (lambda ()
         (setf canvas.width (chain ($ ".canvas-container") (width)))
         (setf canvas.height (chain ($ ".canvas-container") (height)))

         (init)))

      (defun -circle (x y dx dy radius color)
        (setf this.x x)
        (setf this.y y)
        (setf this.dx dx)
        (setf this.dy dy)
        (setf this.radius radius)
        (setf this.min-radius radius)
        (setf this.color (getprop color-array (-math.floor (* (-math.random) color-array.length))))

        (setf this.draw
              (lambda
                  ()
                (c.begin-path)
                (c.arc this.x this.y this.radius 0 (* -math.-p-i 2) f)
                (setf c.fill-style this.color)
                (c.fill)
                nil))

        (setf this.update
              (lambda
                  ()
                (if (or (> (+ this.x this.radius) inner-width) (< (- this.x this.radius) 0))
                    (setf this.dx (- this.dx)))
                (if (or (> (+ this.y this.radius) inner-height) (< (- this.y this.radius) 0))
                    (setf this.dy (- this.dy)))
                (incf this.x this.dx)
                (incf this.y this.dy)

                (cond ((and (< (- mouse.x this.x) 50)
                            (> (- mouse.x this.x) -50)
                            (< (- mouse.y this.y) 50)
                            (> (- mouse.y this.y) -50)
                            )
                       (if (< this.radius max-radius)
                           (incf this.radius)))
                      ((> this.radius this.min-radius)
                       (decf this.radius)))

                (this.draw)
                nil))
        this)

      (defparameter circle-array (make-array))

      (defun init ()
        (setf circle-array (make-array))
        (loop for i from 1 to 50
              do
                 (let* ((radius (+ (* (-math.random) 3) 1))
                        (x (+ (* (-math.random) (- inner-width (* radius 2))) radius))
                        (y (+ (* (-math.random) (- inner-height (* radius 2))) radius))
                        (dx (- (-math.random) 0.5))
                        (dy (- (-math.random) 0.5))
                        )
                   (circle-array.push (new (-circle x y dx dy radius))))))

      (defun animate ()
        (request-animation-frame animate)
        (c.clear-rect 0 0 inner-width inner-height)

        (loop for i in circle-array
              do
                 (i.update)))

      (animate)
      (init)

      )))
