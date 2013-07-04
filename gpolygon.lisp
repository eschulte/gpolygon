(in-package :gpolygon)

;; so we can put js strings into our HTML code
(setf *js-string-delimiter* #\')

(start (make-instance 'hunchentoot:easy-acceptor :port 4242))

(defvar width 198 "Image width.")
(defvar height 300 "Image height.")


;;; Display
(define-easy-handler (main :uri "/main") ()
  (with-html-output-to-string (s)
    (:html
     (:head
      (:script :type "text/javascript"
               (str (ps
                      (defvar current-cnv nil)
                      (defvar target-img (new (-image)))
                      (setf (@ target-img src)
                            ;; need to serve the image from this server
                            "http://cs.unm.edu/~eschulte/data/dock.png")

                      (defvar target-cnv nil)

                      (defun setup ()
                        (setf current-cnv (chain document
                                                 (get-element-by-id "current")
                                                 (get-context "2d")))
                        (setf target-cnv (chain document
                                                (get-element-by-id "target")
                                                (get-context "2d"))))

                      (defun draw (points)
                        (let ((head (chain points pop)))
                          (setf (@ current-cnv fill-style) "#f00")
                          (chain current-cnv (begin-path))
                          (chain current-cnv (move-to (@ head 0) (@ head 1)))
                          (chain points ;; a javascript map, not a CL map
                                 (map (lambda (point)
                                        (chain current-cnv
                                               (line-to (@ point 0)
                                                        (@ point 1))))))
                          (chain current-cnv (close-path))
                          (chain current-cnv (fill))))

                      (setf (@ target-img onload)
                            (lambda () (setup)
                               (chain target-cnv (draw-image target-img 0 0))))
                      
                      (defun data (id)
                        (let* ((canvas (chain document (get-element-by-id id)))
                               (w (@ canvas width))
                               (h (@ canvas height)))
                          (try (chain canvas (get-context "2d")
                                      (get-image-data 0 0 w h)
                                      data)
                               (:catch (e)
                                 (alert
                                  "Firefox is stupid about foreign images")))))

                      (defun score ()
                        (let ((data-current (data "current"))
                              (data-target (data "target"))
                              (difference 0))
                          (dotimes (i (@ data-current length))
                            (incf difference (abs (- (@ data-current i)
                                                     (@ target-current i)))))
                          (alert (+ "difference is " difference))))
                      ))))
     (:body :onload (ps (setup) (draw '((10 10) (25 10) (50 50) (10 25))))
            (:table (:tr (:td "target image")
                         (:td "current " (:a :href "#" :onclick (ps (score))
                                             "score")))
                    (:tr (:td (:canvas :id "target" :width width :height height
                                       :style "border: 1px solid black;"))
                         (:td (:canvas :id "current" :width width :height height
                                       :style "border: 1px solid black;"))))))))
