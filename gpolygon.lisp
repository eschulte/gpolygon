(in-package :gpolygon)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))

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
     (:body :onload (ps (setup) (draw '((10 10) (25 10) (50 50) (10 25))))
            (:table (:tr (:td "target image")
                         (:td "current " (:a :href "#" :onclick (ps (score))
                                             "score")))
                    (:tr (:td (:canvas :id "target" :width width :height height
                                       :style "border: 1px solid black;"))
                         (:td (:canvas :id "current" :width width :height height
                                       :style "border: 1px solid black;"))))))))

(define-easy-handler (eyjafjallajokull :uri "/eyjafjallajokull.png") ()
  (setf (content-type*) "image/png")
  (with-open-file (in "data/eyjafjallajokull.png"
                      :element-type '(unsigned-byte 8))
    (cl-fad:copy-stream in (send-headers))))
