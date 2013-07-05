(in-package :gpolygon)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros)
  (setf *js-string-delimiter* #\"))

(defun serve ()
  "Start serving up pages."
  (start (make-instance 'hunchentoot:easy-acceptor :port 4242)))

(defvar width 216 "Image width.")
(defvar height 162 "Image height.")


;;; Display
(define-easy-handler (main :uri "/") ()
  (with-html-output-to-string (s)
    (:html
     (:head (:script :type "text/javascript" :src "/evolve.js"))
     (:body :onload (ps (setup))
            (:table (:tr (:th "target image") (:th "current "))
                    (:tr (:td (:canvas :id "target" :width width :height height
                                       :style "border: 1px solid black;"))
                         (:td (:canvas :id "current" :width width :height height
                                       :style "border: 1px solid black;"))))
            (:table
             (:tr (:th "Actions")
                  (:td (:a :href "#" :onclick (ps (populate)) "populate") ", "
                       (:a :href "#" :onclick (ps (evolve)) "evolve") ", "
                       (:a :href "#" :onclick (ps (stats)) "stats") ", "
                       (:a :href "#" :onclick (ps (stop)) "stop") ", "
                       (:a :href "#" :onclick (ps (best)) "best")))
             (:tr (:th "best") (:td :id "best" "NA"))
             (:tr (:th "mean") (:td :id "mean" "NA"))
             (:tr (:th "evals") (:td :id "evals" "NA")))))))

(define-easy-handler (eyjafjallajokull :uri "/eyjafjallajokull.png") ()
  (setf (content-type*) "image/png")
  (with-open-file (in "data/eyjafjallajokull.png"
                      :element-type '(unsigned-byte 8))
    (cl-fad:copy-stream in (send-headers))))

(define-easy-handler (evolve-js :uri "/evolve.js") ()
  (setf (content-type*) "text/javascript")
  (ps
(defvar width 216 "Image width.")
(defvar height 162 "Image height.")
(defvar c-cnv nil)
(defvar t-cnv nil)
(defvar target-img (new (-image)))


;;; Functions on page elements
(setf (@ target-img src) "/eyjafjallajokull.png")
(setf (@ target-img onload)
      (lambda () (setup)
         (chain t-cnv (get-context "2d") (draw-image target-img 0 0))))

(defun setup ()
  "Setup the page by assigning both canvas contexts."
  (setf c-cnv (chain document (get-element-by-id "current")))
  (setf t-cnv (chain document (get-element-by-id "target"))))

(defun clear ()
  (chain c-cnv (get-context "2d")
         (clear-rect 0 0 (@ c-cnv width) (@ c-cnv height))))

(defun draw-color (c)
  (+ "rgba(" (aref c 0) ", " (aref c 1) ", " (aref c 2) ", " (aref c 3) ")"))

(defun draw (poly)
  (let* ((points (chain (getprop poly :vertices) (slice 0)))
         (head (chain points pop))
         (ctx (chain c-cnv (get-context "2d"))))
    (setf (@ ctx fill-style) (draw-color (getprop poly :color)))
    (chain ctx (begin-path))
    (chain ctx (move-to (@ head 0) (@ head 1)))
    (chain points ;; a javascript map, not a CL map
           (map (lambda (point)
                  (chain ctx (line-to (@ point 0) (@ point 1))))))
    (chain ctx (close-path))
    (chain ctx (fill))))

(defun data (id)
  (let ((canvas (chain document (get-element-by-id id))))
    (chain canvas (get-context "2d")
           (get-image-data 0 0 (@ canvas width) (@ canvas height)) data)))

(defun score ()
  "Return the difference between the current and target canvases."
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


;;; Functions on individuals
;;
;; Every individual is a hash with fitness and a genome array of
;; polygons.  A polygon is a color and a set of verticies.
;;
(defvar evals 0)
(defvar max-poly-length 6)
(defvar max-genome-start-length 64)

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
(defun copy-ind (ind) (create fit nil genome (chain ind :genome (slice 0))))

(defun evaluate (ind)
  (incf evals)
  (clear)
  (chain ind :genome (map draw))
  (setf (getprop ind :fit) (score))
  ind)

(defun crossover (a b)
  (let* ((ga (chain a :genome))
         (gb (chain b :genome))
         (pt (min (length ga) (length gb))))
    (create fit nil genome
            (append (chain ga (slice 0 pt)) (chain gb (slice pt))))))

(defun tweak (lst)
  (flet ((tweak-num (n) (+ (random n) (/ n 2))))
    (let ((ind (random-ind lst)))
      (if (numberp (aref lst ind))
          (chain lst (splice ind 1 (tweak-num (aref lst ind))))
          (chain lst (splice ind 1 (tweak (aref lst ind))))))))

(defun tweak-poly (poly)
  (tweak (getprop poly (random-elt '(:color :vertices)))))

(defun mutate (ind)
  (let ((i (random-ind (chain ind :genome))))
    (case (random-elt '(:delete :insert :tweak :random))
      (:delete (chain ind :genome (splice i 1)))
      (:insert (chain ind :genome (splice i 0 (poly))))
      (:tweak (tweak-poly (getprop ind :genome i)))
      (:random (new-ind))))
  ind)


;; Evolution functions on populations
(defvar running t)
(defvar pop (make-array))
(defvar pop-size 256)
(defvar tournament-size 2)
(defvar disp-update-delay 2 "Delay in milliseconds to allow display to update.")

(defun fit-sort (a b) (- (getprop a :fit) (getprop b :fit)))
(defun mean (list)
  (let ((total 0))
    (loop :for el :in list :do (incf total el))
    (/ total (length list))))

(defun tournament ()
  ;; for now we can just assume a tournament size of two
  (chain (loop :for i :from 1 :to tournament-size :collect
            (random-elt (chain window pop)))
         (sort fit-sort) 0))

(defun pop-helper (n)
  (when (> n 0)
    (chain window pop (push (evaluate (new-ind))))
    (set-timeout (lambda () (pop-helper (- n 1))) disp-update-delay)))

(defun populate ()
  (set-timeout (lambda () (pop-helper pop-size)) disp-update-delay))

(defun show-scores ()
  (let ((scores ""))
    (dotimes (i pop-size)
      (setf scores (+ scores " " (chain (getprop (chain window pop) i) fit))))
    (alert scores)))

(defun evolve-helper ()
  (when running
    ;; crossover or mutation
    (let ((candidate (case (random-elt '(:crossover :mutation))
                       (:crossover (crossover (tournament) (tournament)))
                       (:mutation (mutate (copy-ind (tournament)))))))
      ;; evaluate
      (evaluate candidate)
      ;; replace a random individual in the population
      (chain window pop (sort fit-sort) (splice -1 1 candidate)))
    ;; recur
    (set-timeout (lambda () (evolve-helper)) disp-update-delay)))

(defun evolve ()
  (setf running t)
  (set-timeout (lambda () (evolve-helper)) disp-update-delay))

(defun stats ()
  "Display stats on the population."
  (let ((scores (chain window pop
                       (sort fit-sort) (map (lambda (it) (getprop it :fit))))))
    (setf
     (chain document (get-element-by-id "evals") inner-h-t-m-l) evals
     (chain document (get-element-by-id "best") inner-h-t-m-l) (aref scores 0)
     (chain document (get-element-by-id "mean") inner-h-t-m-l) (mean scores))
    scores))

(defun stop () (setf running false))
(defun best () (stop) (clear) (evaluate (chain window pop (sort fit-sort) 0)))))
