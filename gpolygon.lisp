(in-package :gpolygon)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *js-string-delimiter* #\"))

(defun serve ()
  "Start serving up pages."
  (start (make-instance 'hunchentoot:easy-acceptor :port 4242)))


;;; Pages
(define-easy-handler (main :uri "/") ()
  (with-html-output-to-string (s)
    (:html
     (:head (:script :type "text/javascript" :src "/evolve.js"))
     (:body :onload (ps (setup))
            (:table (:tr (:th "target image") (:th "current "))
                    (:tr (:td (:canvas :id "target" :onclick (ps (set-image))))
                         (:td (:canvas :id "current" :onclick (ps (get-best))))))
            (:table (:tr (:th "Actions")
                         (:td (loop :for a :in '(populate run stats stop
                                                 show-best do-clear)
                                 :do (htm (:a :href "#" :onclick (ps* (list a))
                                              (str a)) " "))))
                    (loop :for stat :in '("best" "mean" "evals" "length") :do
                       (htm (:tr (:th (str stat)) (:td :id stat "no js")))))))))

(defun serve-img (path stream)
  (setf (content-type*) "image/png")
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (cl-fad:copy-stream in stream)))

(walk-directory "data/img/"
  (lambda (f) (let ((sym (intern (string-upcase (pathname-name f))))
               (uri (format nil "/~a" (file-namestring f))))
           (eval `(define-easy-handler (,sym :uri ,uri) ()
                    (serve-img ,f (send-headers)))))))

(define-easy-handler (evolve-js :uri "/evolve.js") ()
  (setf (content-type*) "text/javascript")
  (ps
(defvar img (new (-image))) (defvar width nil) (defvar height nil)


;;; Page elements
(defun setup ()
  (stats) (set-interval stats 1000)
  (flet ((write (ctx text)
           (setf (@ ctx fill-style) "black"
                 (@ ctx font) "12pt Arial")
           (chain ctx (fill-text text 80 60))))
    (write (chain document (get-element-by-id "target") (get-context "2d"))
           "click to set image")
    (write (chain document (get-element-by-id "current") (get-context "2d"))
           "click to get polygons")))

;; Image server must have enabled CORS -- http://enable-cors.org/
(defun set-image ()
  (let ((canvas (chain document (get-element-by-id "target")))
        (current (chain document (get-element-by-id "current"))))
    (setf (@ img onload)
          (lambda ()
            (setf
             (@ canvas width) (@ img width) (@ canvas height) (@ img height)
             (@ current width) (@ img width) (@ current height) (@ img height)
             width (@ img width) height (@ img height))
            (chain canvas (get-context "2d") (draw-image img 0 0)))
          (@ img cross-origin) "anonymous" ;; w/o this display any, but no data
          (@ img src) (prompt "please enter an image url" "/mona-lisa.png"))))

(defun clear ()
  (chain document (get-element-by-id "current") (get-context "2d")
         (clear-rect 0 0 width height)))

(defun draw-color (c)
  (+ "rgba(" (aref c 0) ", " (aref c 1) ", " (aref c 2) ", " (aref c 3) ")"))

(defun draw (poly)
  (let* ((points (chain (getprop poly :vertices) (slice 0)))
         (head (chain points pop))
         (ctx (chain document (get-element-by-id "current") (get-context "2d"))))
    (setf (@ ctx fill-style) (draw-color (getprop poly :color)))
    (chain ctx (begin-path))
    (chain ctx (move-to (@ head 0) (@ head 1)))
    (chain points (map (lambda (point)
                         (chain ctx (line-to (@ point 0) (@ point 1))))))
    (chain ctx (close-path))
    (chain ctx (fill))))

(defun data (id)
  (chain document (get-element-by-id id) (get-context "2d")
         (get-image-data 0 0 width height) data))

(defun score ()
  (let ((data-current (data "current"))
        (data-target (data "target"))
        (difference 0))
    (dotimes (i (length data-current))
      (incf difference (abs (- (getprop data-current i)
                               (getprop data-target i)))))
    difference))

(defun add-poly () (draw (poly)))
(defun add-ind () (evaluate (new-ind)))
(defun do-clear () (clear))


;;; Individuals
(defvar evals 0)
(defvar max-poly-length 4)
(defvar max-genome-start-length 32)

(defun compose () (list)) ;; needed for loop macro
(defun random-ind (list) (random (length list)))
(defun random-elt (list) (aref list (random-ind list)))
(defun point () (array (random width) (random height)))

(defun random-color ()
  (list (random 256) (random 256) (random 256) (/ (random 100) 100)))

(defun poly ()
  (create color (random-color)
          vertices (loop :for i :from 0 :to (random max-poly-length)
                      :collect (point))))

(defun genome ()
  (loop :for i :from 0 :to (random max-genome-start-length) :collect (poly)))

(defun new-ind () (create fit nil genome (genome)))
(defun copy-poly (poly)
  (create color (chain poly :color (slice 0))
          vertices (chain poly :vertices (map (lambda (v) (chain v (slice 0)))))))
(defun copy-ind (ind)
  (create fit nil genome (chain ind :genome (map copy-poly))))

(defun evaluate (ind)
  (incf evals) (clear) (chain ind :genome (map draw))
  (setf (getprop ind :fit) (score))
  ind)

(defun crossover (a b) ;; one point crossover
  (let ((pt (min (length (getprop a :genome)) (length (getprop b :genome)))))
    (create fit nil genome
            (append (chain (getprop a :genome) (slice 0 pt) (map copy-poly))
                    (chain (getprop b :genome) (slice pt) (map copy-poly))))))

(defun tweak-range (n range)
  ;; bigger or smaller
  (if (> (random) 0.5)
      ;; bigger
      (min (+ n (random (/ n 8))) range)
      ;; smaller
      (max (- n (random (/ n 8))) 0)))

;; seems to work better w/o color tweaks
(defun tweak-poly (poly)
  (if (> (random) 0.5)
      ;; verticies
      (let ((vert (random-elt (getprop poly :vertices))))
        (if (> (random) 0.5)
            ;; width
            (setf (aref vert 0) (tweak-range (aref vert 0) width))
            ;; height
            (setf (aref vert 1) (tweak-range (aref vert 1) height))))
      ;; color
      (let ((pt (random 4)))
        (setf (aref (getprop poly :color) 4)
              (if (= pt 3) 
                  (/ (tweak-range (* 100 (aref (getprop poly :color) pt)) 100)
                     100)
                  (tweak-range (aref (getprop poly :color) pt) 255))))))

(defun merge (a b)
  (create color (getprop a :color)
          vertices (append (getprop a :vertices) (getprop b :vertices))))

(defun mutate (ind)
  (let ((i (random-ind (chain ind :genome))))
    (case (random-elt '(:delete :merge :insert :copy :tweak :swap))
      (:delete (chain ind :genome (splice i 1)))
      (:merge (let ((g (getprop ind :genome))
                    (j (random (- (length (getprop ind :genome)) 1))))
                (chain g (splice j 1 (merge (aref g j) (aref g (+ 1 j)))))))
      (:insert (chain ind :genome (splice i 0 (poly))))
      (:copy (chain ind :genome (splice (random-ind (chain ind :genome)) 0
                                        (copy-poly (getprop ind :genome i)))))
      (:tweak (tweak-poly (getprop ind :genome i)))
      (:swap (let ((cp (copy-poly (random-elt (chain ind :genome)))))
               (chain ind :genome (splice i 1 cp))))))
  ind)


;; Populations
(defvar running t)
(defvar pop (make-array))
(defvar pop-size 128)
(defvar tournament-size 2)
(defvar disp-update-delay 2 "Delay in milliseconds to allow display to update.")

(defun fit-sort (a b) (- (getprop a :fit) (getprop b :fit)))
(defun mean (l) (/ (loop :for el :in l :sum el) (length l)))
(defun mean-length ()
  (mean (chain window pop (map (lambda (it) (length (getprop it :genome)))))))

(defun tournament ()
  (chain (loop :for i :from 1 :to tournament-size :collect
            (random-elt (chain window pop)))
         (sort fit-sort) 0))

(defun pop-helper (n)
  (when (and running (> n 0))
    (chain window pop (push (evaluate (new-ind))))
    (set-timeout (lambda () (pop-helper (- n 1))) disp-update-delay)))
(defun populate () (set-timeout (lambda () (pop-helper pop-size)) disp-update-delay))

(defun run-helper ()
  (when running
    (chain window pop (sort fit-sort)
           (splice -1 1 (evaluate
                         (mutate (if (= 0 (random 2))
                                     (copy-ind (tournament))
                                     (crossover (tournament) (tournament)))))))
    (set-timeout run-helper disp-update-delay)))
(defun run () (setf running t) (set-timeout run-helper disp-update-delay))

(defun stats ()
  (let ((scores (chain window pop
                       (sort fit-sort) (map (lambda (it) (getprop it :fit))))))
    (setf
     (chain document (get-element-by-id "evals") inner-h-t-m-l) evals
     (chain document (get-element-by-id "best") inner-h-t-m-l) (aref scores 0)
     (chain document (get-element-by-id "mean") inner-h-t-m-l) (mean scores)
     (chain document (get-element-by-id "length") inner-h-t-m-l) (mean-length))
    scores))

(defun stop () (setf running false))
(defun best () (chain window pop (sort fit-sort) 0))
(defun show-best () (stop) (clear) (chain (best) :genome (map draw)))
(defun get-best ()
  (chain window (open (+ "data:text/JSON;base64,"
                         (btoa (chain -j-s-o-n (stringify (best))))))))))
