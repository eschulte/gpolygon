(in-package :gpolygon)

;; so we can put js strings into our HTML code
(setf *js-string-delimiter* #\')

(start (make-instance 'hunchentoot:easy-acceptor :port 4242))

;;; Drawing functions
(define-easy-handler (draw :uri "/draw") ()
  "Draw a simple rectangle."
  (with-html-output-to-string (s)
    (:html
     (:head
      (:script :type "text/javascript"
               (str (ps (defun draw (points)
                          (let ((c2 (chain document
                                           (get-element-by-id "main")
                                           (get-context "2d")))
                                (head (chain points pop)))
                            (setf (@ c2 fill-style) "#f00")
                            (chain c2 (begin-path))
                            (chain c2 (move-to (@ head 0) (@ head 1)))
                            (chain points ;; a javascript map, not a CL map
                                   (map (lambda (point)
                                          (chain c2 (line-to (@ point 0)
                                                             (@ point 1))))))
                            (chain c2 (close-path))
                            (chain c2 (fill))))))))
     (:body :onload (ps (draw '((10 10) (25 10) (50 50) (10 25))))
            (:canvas :id "main" :width 100 :height 100
                     :style "border: 1px solid black;")))))
