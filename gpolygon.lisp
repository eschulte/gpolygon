(in-package :gpolygon)

;; so we can put js strings into our HTML code
(setf *js-string-delimiter* #\')

(start (make-instance 'hunchentoot:easy-acceptor :port 4242))


;; two simple functions from the tutorial
(define-easy-handler (tutorial1 :uri "/tutorial1") ()
  (with-html-output-to-string (s)
    (:html
     (:head (:title "tutorial 1 (gpolygon)"))
     (:body (:h2 "example the first")
            "please click on this thing"
            :br
            (:a :href "#" :onclick (ps (alert "pooped on you"))
                "I am the thing to be clicked")))))

(define-easy-handler (tutorial2 :uri "/tutorial2") ()
  (with-html-output-to-string (s)
    (:html
     (:head (:title "tut 2"))
     (:script :type "text/javascript"
              (str (ps (defun greet-callback ()
                         (alert "fart in your direction"))))))
    (:body
     (:h2 "paren tutorial the second")
     "here's some more stuff on which to click"
     (:a :href "#" :onclick (ps (greet-callback)) "me me me"))))


;;; Drawing functions
(define-easy-handler (draw :uri "/draw") ()
  "Draw a simple rectangle."
  (with-html-output-to-string (s)
    (:html
     (:head
      (:script :type "text/javascript"
               (str (ps (defun draw ()
                          (let ((c2 (chain document
                                           (get-element-by-id "main")
                                           (get-context "2d"))))
                            (setf (@ c2 fill-style) "#f00")
                            (chain c2 (fill-rect 10 10 80 80))))))))
     (:body :onload (ps (draw))
            (:canvas :id "main" :width 100 :height 100
                     :style "border: 1px solid black;")))))
