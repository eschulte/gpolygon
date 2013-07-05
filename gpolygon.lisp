(in-package :gpolygon)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))

(setf *js-string-delimiter* #\")

(eval-when (:execute)
  (start (make-instance 'hunchentoot:easy-acceptor :port 4242)))

(defvar width 216 "Image width.")
(defvar height 162 "Image height.")
(defvar js (ps-compile-file "gpolygon.parenscript"))


;;; Display
(define-easy-handler (main :uri "/") ()
  (with-html-output-to-string (s)
    (:html
     (:head (:script :type "text/javascript" (str js)))
     (:body :onload (ps (setup))
            (:table (:tr (:td "target image") (:td "current "))
                    (:tr (:td (:canvas :id "target" :width width :height height
                                       :style "border: 1px solid black;"))
                         (:td (:canvas :id "current" :width width :height height
                                       :style "border: 1px solid black;"))))
            (:a :href "#" :onclick (ps (add-poly)) "add polygon") :br
            (:a :href "#" :onclick (ps (do-clear)) "clear") :br
            (:a :href "#" :onclick (ps (alert (score))) "score")))))

(define-easy-handler (eyjafjallajokull :uri "/eyjafjallajokull.png") ()
  (setf (content-type*) "image/png")
  (with-open-file (in "data/eyjafjallajokull.png"
                      :element-type '(unsigned-byte 8))
    (cl-fad:copy-stream in (send-headers))))
