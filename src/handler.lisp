(defpackage #:handler
  (:use #:cl #:hunchentoot #:parenscript #:cl-fad #:cl-who))

(in-package #:handler)

(defmacro+ps clog (&body body)
  `(chain console (log ,@body)))

(defmacro+ps @@ (&body body)
    `(chain ,@body))

(defmacro+ps $. (&body body)
  `(chain $ ,@body))

(defmacro+ps $$ (selector &body body)
  `(chain ($ ,selector) ,@body))

(defmacro+ps \ (args &body body)
  `(lambda ,args ,@body))

;; TODO
;; on-event macro
;; on document ready macro


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
       (:link :rel "stylesheet"
              :href "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"
              :integrity "sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T"
              :crossorigin "anonymous")

       (:link :rel "stylesheet" :href "/css/fontawesome-all.css")
       (:link :rel "icon" :href "favicon.ico")
       (:link :rel "stylesheet" :type "text/css" :href "css/main.css")
       (:title ,@title)
       (:script :src "https://code.jquery.com/jquery-3.2.1.min.js")
       (:script :type "text/javascript"
                     (str
                      (ps
                        (defun get-css-value-from-var (css-var)
                          (chain (get-computed-style document.document-element)
                                 (get-property-value css-var)))
                        (let ((navbar-height (parse-int (get-css-value-from-var "--navbar-height"))))
                          (defun smooth-scroll (location &optional (nav nil))
                            (let ((height (if nav
                                              (- ($$ location (offset) top) navbar-height)
                                              ($$ location (offset) top)
                                              )))
                              ($$ "html, body"
                                (animate
                                 (create :scroll-top height)
                                 500)))
                            f)
                          ;; FIXME add callback option to reduce savage accordian scrolling
                          ;; according collapsing is still broken
                          (defun chunky-scroll (location &optional (nav nil))
                            (let ((height (if nav
                                              (- ($$ location (offset) top) navbar-height)
                                              ($$ location (offset) top)
                                              )))
                              ($$ "html, body"
                                (animate
                                 (create :scroll-top height)
                                 200)))
                            f))

                        ($$ document (on "click" "a[href^=\"#\"]"
                                         (\ (event)
                                            (@@ event (prevent-default))
                                            (smooth-scroll ($. (attr this "href")) t)))))))
       )
      (:body :class "container-fluid w-100 p-0"
             ,@body

             (:script :src "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.12.9/umd/popper.min.js"
                      :integrity "sha384-ApNbgh9B+Y1QKtv3Rn7W3mgPxhU9K/ScQsAP7hUibX39j7fakFPskvXusvfa0b4Q"
                      :crossorigin "anonymous")
             (:script :src "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/js/bootstrap.min.js"
                      :integrity "sha384-JZR6Spejh4U02d8jOt6vLEHfe/JQGiRRSQQxSfFWpi1MquVdAyjUar5+76PVCmYl"
                      :crossorigin "anonymous")))))

(defun profile-handler ()
  (page-template (:title "Welcom to the Lounge")
    (with-html-output (*standard-output*)

      (:header
       :class "container-fluid squeeze-out fixed-top"
       (:div :class "d-flex flex-row py-1"
             (:a :class "nav-link offset-lg-2 offset-md-1" :href "#home" "HOME")
             (:a :class "nav-link" :href "#about" "ABOUT")
             (:a :class "nav-link" :href "#portfolio" "PORTFOLIO")
             (:a :class "nav-link" :href "#contact" "CONTACT"))

       (:script :type "text/javascript"
                (str (ps
                       ($$ window (scroll
                                   (\ ()
                                      (if (>=
                                           ($$ window (scroll-top))
                                           (- (@ window inner-height) 200))
                                          ($$ "header" (remove-class "squeeze-out"))
                                          ($$ "header" (add-class "squeeze-out")))))))))
       )

      (:section
       :id "home" :class "home d-flex flex-row"
       (:div :class "canvas-container"
             (:canvas :class "canvas" "Please use a modern browser to view this site ):")
             (:script
              :type "text/javascript"
              (str (home-canvas-ps))))

       (:div :class "col align-self-center text-center"

             (:div :style "font-size: 2.3rem" "Hello, I'm "
                   (:b :class "my-name text-nowrap" "Evan MacTaggart."))

             (:div
              :style "font-size: 2.5rem"
              "A backpacker and programmer.")

             (:button :class "garbage-btn btn btn-primary btn-lg m-3 px-3 text-light"
                      :onclick (ps (smooth-scroll "#about"))
                      (:div :class "align-middle"
                            (:span :class "align-middle" :style "height: 100%;" "See more" )
                            (:i :class "fa fa-arrow-circle-right rotate-90-animation ml-2 align-middle"))
                      )))

      (:section :id "about" :class "about py-5"
                (:div :class "container text-center"

                      (:div :class "flex-row justify-content-center mb-3"
                            (:h1 (:b "ABOUT"))
                            (:div :class "d-flex justify-content-center"
                                  (:div :class "underline-bar")))

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
                                  (:p "I'm only human but continually working towards attaining wizard status."))

                            (:div :class "col-lg-3 col-md-6 col-xs-12"
                                  (:div
                                   (:i :class "fa fa-5x fa-cogs")
                                   (:h3 "Simplicity"))
                                  (:p "Simplicity is the key to sane development. Likewise I prefer to avoid over-designing and over-engineering things.")))


                      (:div :class "d-flex justify-content-center"
                            (:div :class "portrait-container"
                                  ;; FIXME clip-path: circle(40%) https://developer.mozilla.org/en-US/docs/Web/CSS/clip-path
                                  (:img :class "portrait" :src "/resources/profile-photos/cooking-ahh.jpg")))

                      (:div :class "py-3"
                            "I'm a developer comfortable working in a plethora of technologies and environments and not afraid to learn something new. Have a look at some of my skills!")


                      (let ((professional
                              '(:id "pro"
                                :title "Professional"
                                :desc "The technologies I've used in the majority of my professional experience."
                                :items (
                                         (:name "Java"
                                          :img "/resources/logos/java.png"
                                          :desc "With Java I have developed backend services for various applications as well as some internal business facing GUI applications. Various university courses also used Java as a point of focus for OOP.")
                                         (:name "JavaScript"
                                          :img "/resources/logos/javascript.png"
                                          :desc "Javascript is unavoidable at this point. Through school and various work experience I have used Javascript for front end and back end, primarily the former.")
                                         (:name "Spring & Spring Boot"
                                          :img "/resources/logos/spring.png"
                                          :desc "The core of my Java experience exists inside the context of Spring. I have developed RESTful services, cron jobs, web applications with spring.")
                                         (:name "SQL"
                                          :img "/resources/logos/mysql.png"
                                          :desc "Starting in school but carrying forward to professional experience SQL has been a staple in my DB experience. The majority of projects I have been involved with have made use of a relational databases.")
                                         (:name "Angular"
                                          :img "/resources/logos/angular.png"
                                          :desc "My first professional web development experience out of university was Angular. My experience primarily exists with version 2+, but have had a short time with the original. With Angular I have built various single-page web applications, both customer and business facing.")
                                         (:name "Web Development"
                                          :img "/resources/logos/html-css-js.png"
                                          :desc "The web being such an incredibly useful medium, web development is likewise a common task. Mobile responsive design is also in the realm of my expertise, as hopefully this page can attest."))))
                            (hobbies
                              '(:id "hobbies"
                                :title "Hobbies"
                                :desc "Some things I'm currently dabbling in."
                                :items
                                ((:name "Common Lisp"
                                   :img "/resources/logos/lisp.png"
                                   :desc "I built this website using Common Lisp, take a look at github to see it's current state 😬. My experience has been short but enlightening and very enjoyable once climbing over some initial hurdles. I have intentions of continuing my exploration through this humble language.")
                                  (:name "Statistics"
                                   :img "/resources/logos/statistics.png"
                                   :desc "I'm slowly working at understanding the broad topic of statistics and data analysis and all that they encompass. Likewise model generation and machine learning logically will come afterwards. I intend to make use of Python's Pandas and NumPy libraries, as well as JavaScripts D3 for visual representation.")
                                  (:name "Emacs"
                                   :img "/resources/logos/emacs.png"
                                   :desc "After succumbing to the dark side I transitioned from Vim to Emacs through Spacemacs, which I'm currently still using as my editor of choice. Emacs was the monumental driver towards learning Lisp like languages.")
                                 (:name "Docker"
                                  :img "/resources/logos/docker.png"
                                  :desc "Docker is a technology I have more recently been diving into for the sake of devops and clean working environment purposes."
                                  ))))
                            (generic-dev
                              '(:id "generic"
                                :title "Generic"
                                :desc "Various technologies I've used both in and out of my professional experience."
                                :items
                                ((:name "Linux"
                                   :img "/resources/logos/linux.png"
                                   :desc "Since my introduction to using Linux in early university I have gradually transitioned into using it full time as my default OS. At this instant I'm running Fedora but have dabbled in Ubuntu, debian, and CentOS in the past. I have even built my own linux kernel from scratch through various tutorials! I have also dabbled in MacOS, and have a dual boot to Windows for other occasional uses."
                                   )
                                  (:name "Devops"
                                   :img "/resources/logos/devops.jpg"
                                   :desc "Not being totally new to web development, but being relatively new to hosting my own services, devops is an area of interest of mine. Having plenty of linux experience, and now freshly, an understanding docker, I am digging deeper into the processes involed with devops automation and continuous integration, which I have made of plenty use of, in previous work experience.")
                                  (:name "Git"
                                   :img "/resources/logos/git.png"
                                   :desc "Since early on in my development career Git has been the primary choice of version control. Being a command line warrior I like to think that I have a intermediate-advanced level of understanding of git, without digging into the sublevel commands git is comprised of. My git client of choice is Magit, an Emacs plugin surprisingly /s. I have also used Mercurial in a professional environment as well.")
                                  (:name "Scrum"
                                   :img "/resources/logos/scrum.png"
                                   :desc "Through previous corporate work experience, I had the pleasure of collaborating on a few teams where Scrum was used effectively, dynamically, and autonomously as each team saw fit. This experience goes along with the comprimise and coordination between multiple Scrum teams driven towards a larger, single, and encompassing goal. The teams were sized from 3 to 10 people.")
                                  (:name "Testing"
                                   :img "/resources/logos/tdd.png"
                                   :desc "Starting in my student developer work terms testing has been a strong area of interest. I have professional experience with test driven development which ranges from unit tests, to integration tests, to automated UI tests, to building the initialy framework for automated integration testing for a dynamically selected environement.")
                                 (:name "Security"
                                  :img "/resources/logos/security.svg"
                                  :desc "I like to think that I'm not as security ignorant as most."))))
                            (misc
                              '(:id "misc"
                                :title "Miscellaneous"
                                :desc "Stuff I've played around with in the past for various reasons, willingly or not."
                                :items
                                ((:name "Haskell"
                                   ;; :img "/resources/logos/haskell.svg"
                                   :desc "Back in the days of FP hype I was intrigued by Haskell and absolutely battered (mentally) by the difference in programming style. Dabbling on and off over a year or so, having not really produced anything of importance, I did gain a solid understanding of what it means to be purely functional. I have a lot of respect for this language and appreciate it's brick wall like embrace into the world of FP")
                                  (:name "Clojure"
                                   ;; :img "/resources/logos/clojure.png"
                                   :desc "After having been shown the light of functional programming from Haskell and having a solid understanding of Java and the JVM, I ventured towards into Rich Hickey's child, Clojure / Clojurescript. Clojure being my first Lisp  was a surprisingly smooth introduction to a more dynamic functional programming language. ")
                                 (:name "Python"
                                  ;; :img "/resources/logos/python.png"
                                  :desc "My Python experience stems primarily from use for various school projects. One for a cryptography/network security course where I build a program to encrypt/decrypt. I have dabbled in Django and have used various libraries for simple servers and etc. I have ambitions of getting into data analysis using Pandas, NumPy, PyNotebook, and Python's other vetted libraries.")
                                  (:name "Vim"
                                   :img ""
                                   :desc "As a young soldier in the editor war I eventually found myself having to choose a side, my initial choice being Vim for, perhaps reasons unknown, other than fear of the rumored \"Emac's Pinky\".")
                                  (:name "C & C++"
                                   :img ""
                                   :desc "Like most other once-upon-a-time-university-students I have seen the likes of C and C++ primarily through use in school. C++ was my first and ultimately led me to continuing my education in the field of software. Other related areas of interest are that of security and the decompilation of binaries to find vulnerabilities in software.")
                                  (:name "SAP Hana"
                                   :img ""
                                   :desc "On and off I have had some small endeavours into the proprietary world of SAP Hana building and modifying data models for various services.")
                                  (:name "C#"
                                   :img ""
                                   :desc "During a couple work terms I had worked with C# on some web services for internal business use. I have also developed a mobile, networked, game with the Unity engine for my final year software engineering project.")
                                  (:name "PHP"
                                   :img ""
                                   :desc "Although not a personal favorite, with PHP I was lucky enough to have some experience building a tool and web interface to archive VM images ranging from ~40GBs in size.")))))

                        (labels ((card (skill)
                                   (htm
                                    (:div
                                     :class "card col-12 col-md-6 col-lg-4 col-xl-3 p-1"
                                     :style "border: 0;"

                                     (:div
                                      :class "card-img-top text-center pb-2"
                                      :style ""

                                      (:img
                                       :style "max-height: 100px; max-width: 100%"
                                       :src (getf skill :img)))

                                     (:div
                                      :class "card-body"
                                      :style ""
                                      (:h3 :class "card-title"
                                            (str (getf skill :name)))
                                      (:div :class "card-text text-left"
                                       (str (getf skill :desc)))))))
                                 (card-container (skills)
                                   (htm (:div :class "container"
                                              (:div :class "d-flex row"
                                                    (loop for skill in skills do
                                                      (card skill))))))
                                 (accordian-item (&key id title desc items)
                                   (htm
                                    (:div :class "card"
                                          :style "border: 0; border-bottom: solid black 1px;"
                                          (:div :class "card-header d-flex align-items-center"
                                                :id (format nil "heading-~a" id)
                                                :style "height: 100px; background: var(--white)"
                                                :data-toggle "collapse"
                                                :data-target (format nil "#collapse-~a" id)
                                                :aria-expanded "false"
                                                (:h3 :class "mb-0"
                                                     (str title))
                                                (:i :class "ml-auto mt-2 fa fa-2x fa-chevron-down rotate-icon"))

                                          (:div :id (format nil "collapse-~a" id)
                                                :class "collapse"
                                                :data-parent "#skill-accordian"
                                                (:div :class "pt-2 pb-3 text-left"
                                                      :style "color: var(--grey-500)"
                                                      (str desc))
                                                (:div :class "card-body p-0 p-lg-3"
                                                      (card-container items))
                                                (:div
                                                 :style "background: var(--grey-100);"
                                                 :data-target (format nil "#collapse-~a" id)
                                                 :onclick
                                                 (ps
                                                   (let ((el (lisp (format nil "#collapse-~a" id))))
                                                     (chunky-scroll "#skill-accordian" t)
                                                     (@@
                                                      ($ el)
                                                      (collapse "hide"))))
                                                 (:i :class "ml-auto mt-2 fa fa-2x fa-chevron-up")))))))

                          (htm
                           (:div :id "skill-accordian"
                                 :class "m-3"
                                 (apply #'accordian-item professional)
                                 (apply #'accordian-item hobbies)
                                 (apply #'accordian-item generic-dev)
                                 (apply #'accordian-item misc)))))))

      (:hr)

      (:section :id "portfolio" :class "portfolio py-5 text-center"

                (:div :class "d-flex flex-row justify-content-center"
                      (:div
                       (:div :class "flex-row justify-content-center mb-3"
                             (:h1 (:b "WORK"))
                             (:div :class "d-flex justify-content-center"
                                   (:div :class "underline-bar")))

                       (:p "Check me out on other platforms")

                       (:div :class "row justify-content-center m-4"
                             (:div
                              (:div
                               (:a :class "m-2" :href "https://github.com/emactaggart"
                                   (:i :class "wonk fab fa-8x fa-github"))

                               (:a :class "m-2" :href "https://www.linkedin.com/in/evan-mactaggart-1a7826122"
                                   (:i :class "fab fa-8x fa-linkedin"))

                               (:a :class "m-2 text-middle"
                                   :href "https://drive.google.com/file/d/1sZk9o56LG1O-f8gmzVKcrvowXqAjBiCU/view"
                                   (:i :class "fa fa-8x fa-file-pdf")))))
                       (:div :class "alert alert-warning"
                             (:i :class "fa fa-lg fa-hard-hat")
                             " My projects are underway, come back in the later to check them out! "
                             (:i :class "fa fa-lg fa-tools")))
                      ))

      (:section :id "contact" :class "contact py-5 h-100"

                (:div :class "h-100 container d-flex justify-content-center"
                      (:div :class "align-self-center col-12 col-sm-10 col-md-9 text-center"

                            (:h1 (:b "CONTACT"))
                            (:div :class "row justify-content-center"
                                  (:div :class "underline-bar"))


                            (:div :class "py-3 text-accent"
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
                                                                            ($$ "#message-count"
                                                                                   (text (- (lisp *message-length*)
                                                                                            (@ event target text-length)
                                                                                            1))))
                                                                          event)))

                                                    :maxlength *message-length*
                                                    :required t
                                                    )

                                         (:small
                                          :class "remaining"
                                          "Characters remaining: "
                                          (:span :id "message-count" )
                                          (:script :type "text/javascript"
                                                   (str (ps
                                                          ((lambda ()
                                                             (chain
                                                              ($ document)
                                                              (ready
                                                               (lambda ()
                                                                 (let ((message-length ($$ "#message"
                                                                                              (val) length))))

                                                                 ($$ "#message-count"
                                                                        (text (- (lisp *message-length*)
                                                                                 message-length)))))))))))))

                                   (:div :id "contact-success" :class "alert alert-success mt-2 d-none"
                                         "Message sent!")
                                   (:div :id "contact-error" :class "alert alert-danger mt-2 d-none"
                                         "Sending message was unsuccessful. Feel free to contact me directly at "
                                         (:a :class "alert-link" :href "mailto: evan.mactaggart@gmail.com" "evan.mactaggart@gmail.com"))

                                   (:button :class "garbage-btn btn btn-primary mt-1 float-right"
                                            :type "submit"
                                            :id "submit"
                                            (:b :class "mx-3" "Submit"))

                                   (:script :type "text/javascript"
                                            (str (ps
                                                   ($$ "#my-form"
                                                          (submit
                                                           (lambda (event)
                                                             (@@ event (prevent-default))

                                                             (let* (($form ($ this))
                                                                    (url (chain $form (attr "action")))
                                                                    (form-data (create :name ($$ "#name" (val))
                                                                                       :email ($$ "#email" (val))
                                                                                       :message ($$ "#message" (val)))))

                                                               (chain $
                                                                      (post
                                                                       "send-message"
                                                                       form-data
                                                                       (lambda (data)
                                                                         ($$ "#name" (val ""))
                                                                         ($$ "#email" (val ""))
                                                                         ($$ "#message" (val ""))
                                                                         ($$ "#contact-success" (remove-class "d-none"))
                                                                         ($$ "#contact-error" (add-class "d-none"))
                                                                         ))
                                                                      (fail
                                                                       (lambda ()
                                                                         ($$ "#contact-success" (add-class "d-none"))
                                                                         ($$ "#contact-error" (remove-class "d-none"))
                                                                         ))
                                                                      ))

                                                             f)))

                                                   ))))
                            )))

      (:footer :class "footer py-5 position-relative"

               (:a :href "#home" :style "color: var(--white)"
                   :onclick (ps (funcall
                                 (\ (event)
                                    (@@ event (prevent-default))
                                    (smooth-scroll "#home"))
                                 event))
                   (:div :class "top-button text-center position-absolute"
                         (:i :class "mt-2 fa fa-2x fa-arrow-circle-up")))

               (:div :class "d-flex flex-row justify-content-center py-3"
                     (:a :class "p-2" :href "https://github.com/emactaggart"
                         (:i :class "fab fa-3x fa-github-square p-2"))
                     (:a :class "p-2" :href "https://www.linkedin.com/in/evan-mactaggart-1a7826122"
                         (:i :class "fab fa-3x fa-linkedin p-2")))

               (:div :class "d-flex flex-row justify-content-center py-3"
                     (:small :style "color: var(--grey-400)" "EVAN MACTAGGART"
                             (:span :style "color: var(--accent-1)" " 2019"))))

      )))

(defun home-canvas-ps ()
  (ps

    (let ((canvas (chain document (query-selector "canvas"))))
      (var item-count 20)
      (var c (chain canvas (get-context "2d")))
      (var max-radius 40)
      (var min-radius 2)
      (var mouse (create :x undefined :y undefined))
      (var css-vars (make-array "--accent-1" "--accent-2" "--accent-3" "--accent-4" ))
      (var color-array (chain css-vars
                              (map get-css-value-from-var)))

      (window.add-event-listener
       "mousemove"
       (lambda (event)
         (setf mouse.x event.x)
         (setf mouse.y event.y)))

      (defun -circle (x y dx dy radius color)
        (setf this.x x)
        (setf this.y y)
        (setf this.dx dx)
        (setf this.dy dy)
        (setf this.radius radius)
        (setf this.min-radius radius)
        (setf this.color (getprop color-array (-math.floor (* (-math.random) color-array.length))))

        (setf this.draw
              (lambda ()
                (c.begin-path)
                (c.arc this.x this.y this.radius 0 (* -math.-p-i 2) f)
                (setf c.fill-style this.color)
                (c.fill)
                nil))

        (setf this.update
              (lambda ()
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

      (defun resize-canvas ()
        (setf canvas.width ($$ ".home .canvas-container" (width)))
        (setf canvas.height ($$ ".home .canvas-container" (height))))

      (defun init ()
        (resize-canvas)
        (setf circle-array (make-array))
        (loop for i from 1 to item-count
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

      (window.add-event-listener
       "resize"
       (lambda ()
         (resize-canvas))))

    (chain
     ($ document)
     (ready
      (lambda ()
        (init))))))

