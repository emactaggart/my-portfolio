(defpackage handler
  (:use :cl :hunchentoot :parenscript :cl-fad :cl-who))

(in-package :handler)

;; (defpackage "ps-tutorial"
;;   (:use :common-lisp :cl-fad :cl-who :hunchentoot :parenscript :prove))
;; (in-package "ps-tutorial")

(setf cl-who:*attribute-quote-char* #\")



(setf *show-lisp-errors-p* t
      *show-lisp-backtraces-p* t)
;; (stop wow:**ACCEPTOR**)

(define-easy-handler (example1 :uri "/example1") ()
  (with-html-output-to-string (s)
    (:html
     (:head (:title "Parenscript tutorial: 1st example"))
     (:body (:h2 "Parenscript tutorial: 1st example")
            "Please click the link below." :br
            (:a :href "#" :onclick (ps (alert "wow amazing"))
                "Hello World")))))

(define-easy-handler (example2 :uri "/example2") ()
  (with-html-output-to-string (s)
    (:html
     (:head
      (:title "Parenscript tutorial: 2nd example")
      (:script :type "text/javascript"
               (str (ps (defun greeting-callback ()
                         (alert "Hellow World"))))))
     (:body
      (:h2 "Parenscript tutorial: 2nd example")
      (:a :href "#" :onclick (ps (greeting-callback))
          "Hello World")))))

(define-easy-handler (example3 :uri "/example3.js") ()
  (setf (content-type*) "tet/javascript")
  (ps
    (defun greeting-callback ()
      (alert "Hello World"))))


(defvar *slideshows* (make-hash-table :test 'equalp))

(defun add-slideshow (slideshow-name image-folder)
  (setf (gethash slideshow-name *slideshows*) image-folder)
  (push (create-folder-dispatcher-and-handler
         (format nil "/slideshow-images/~a/" slideshow-name)
         image-folder)
        *dispatch-table*))


;; PLAYGROUND

;; (defun do-push (endpoint directory)
;;     (push (create-folder-dispatcher-and-handler
;;            endpoint directory)
;;      *dispatch-table*))

;; (push (create-folder-dispatcher-and-handler
;;        "/images/" "/home/evan/Downloads/my-parenscript-project/test/")
;;       *dispatch-table*)

;; (push (create-folder-dispatcher-and-handler
;;        "/lolcat/" "/home/evan/Downloads/my-parenscript-project/lolcats/")
;;       *dispatch-table*)

;; (define-easy-handler (my-image-handler :uri "/my-image-handler") ()
;;   (with-html-output-to-string (s)
;;     (:html
;;      (:div
;;       (:img :src "/test2/stock.jpeg")
;;       (:p "wow")))))

;; (define-easy-handler (my-handler :uri "/wow")
;;     ()
;;   (with-html-output-to-string (s)
;;     (:html
;;      (:h1 "wow")
;;      (:img :src "/test2/stock.jpeg"))))

;; END PLAYGROUND

(add-slideshow "lolcat" "/home/evan/Downloads/my-parenscript-project/lolcats/")
(add-slideshow "lolrus" "/home/evan/Downloads/my-parenscript-project/lolruses/")

(defmacro+ps slideshow-image-uri (slideshow-name image-file)
  `(concatenate 'string "/slideshow-images/" ,slideshow-name "/" ,image-file))

(list-directory "~/Downloads/my-parenscript-project/lolcats")




(defun slideshow-handler ()
  (cl-ppcre:register-groups-bind (slideshow-name)
      ("/slideshows/(.*)" (script-name*))
    (let* ((images (mapcar
                    (lambda (i) (url-encode (file-namestring i)))
                    (list-directory
                     (or (gethash slideshow-name *slideshows*)
                         (progn (setf (return-code*) 404)
                                (return-from slideshow-handler))))))
           (current-image-index
             (or (position (url-encode (or (get-parameter "image") ""))
                           images
                           :test #'equalp)
                 0))
           (previous-image-index (max 0
                                      (1- current-image-index)))
           (next-image-index (min (1- (length images))
                                  (1+ current-image-index))))
      (with-html-output-to-string (s)
        (:html
         (:head
          (:title "Parenscript slideshow")
          (:script
           :type "text/javascript"
           (str
            (ps*
             `(progn
                (var *slideshow-name* ,slideshow-name)
                (var *images* (array ,@images))
                (var *current-image-index* ,current-image-index)))))
          (:script :type "text/javascript" :src "/slideshow.js"))
         (:body
          (:div :id "slideshow-container"
                :style "width:100%;text-align:center"
                (:img :id "slideshow-img-object"
                      :src (slideshow-image-uri
                            slideshow-name
                            (elt images current-image-index)))
                :br
                (:a :href (format nil "/slideshows/~a?image=~a"
                                  slideshow-name
                                  (elt images previous-image-index))
                    :onclick (ps (previous-image) (return false))
                    "Previous")
                " "
                (:a :href (format nil "/slideshows/~a?image=~a"
                                  slideshow-name
                                  (elt images next-image-index))
                    :onclick (ps (next-image) (return false))
                    "Next"))))))))

(push (create-prefix-dispatcher "/slideshows/" 'slideshow-handler)
      *dispatch-table*)

(define-easy-handler (js-slideshow :uri "/slideshow.js") ()
  (setf (content-type*) "text/javascript")
  (ps
    (define-symbol-macro fragment-identifier (@ window location hash))

    (defun show-image-number (image-index)
      (let ((image-name (aref *images* (setf *current-image-index* image-index))))
        (setf (chain document (get-element-by-id "slideshow-img-object") src)
              (slideshow-image-uri *slideshow-name* image-name)
              fragment-identifier
              image-name)))

    (defun previous-image ()
      (when (> *current-image-index* 0)
        (show-image-number (1- *current-image-index*))))

    (defun next-image ()
      (when (< *current-image-index* (1- (getprop *images* 'length)))
        (show-image-number (1+ *current-image-index*))))

    

    ;; use fragment identifiers to allow bookmarking
    (setf (getprop window 'onload)
          (lambda ()
            (when fragment-identifier
              (let ((image-name (chain fragment-identifier (slice 1))))
                (dotimes (i (length *images*))
                  (when (string= image-name (aref *images* i))
                    (show-image-number i)))))))))

(push (create-prefix-dispatcher "/test/" 'test-handler)
      *dispatch-table*)

(defun test-handler ()
  (cl-ppcre:register-groups-bind (slideshow-name)
      ("/slideshows/(.*)" (script-name*))
    slideshow-name)
  )


