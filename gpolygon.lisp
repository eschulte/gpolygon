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
            (:table (:tr (:td "target image") (:td "current "))
                    (:tr (:td (:canvas :id "target" :width width :height height
                                       :style "border: 1px solid black;"))
                         (:td (:canvas :id "current" :width width :height height
                                       :style "border: 1px solid black;"))))
            (:a :href "#" :onclick (ps (populate (lambda () (evolve best))))
                "evolve")))))

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
  (clear)
  (chain (@ ind genome) (map draw))
  (setf (@ ind fit) (score))
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
    (case (random-elt '(:delete :insert :tweak))
      (:delete (chain ind :genome (splice i 1)))
      (:insert (chain ind :genome (splice i 0 (poly))))
      (:tweak (tweak-poly (getprop ind :genome i))))))


;; Evolution functions on populations
(defvar pop (make-array))
(defvar pop-size 124)
(defvar max-evals 1024)

(defun pop-helper (n cb)
  (if (> n 0)
      (progn
        (chain window pop (push (evaluate (new-ind))))
        (set-timeout (lambda () (pop-helper (- n 1) cb)) 20))
      (cb)))

(defun populate (cb)
  (set-timeout (lambda () (pop-helper pop-size cb)) 20))

(defun show-scores ()
  (let ((scores ""))
    (dotimes (i pop-size)
      (setf scores (+ scores " " (chain (getprop (chain window pop) i) fit))))
    (alert scores)))

;; TODO: wrap this and pop-helper in a macro
(defun evolve-helper (n cb)
  (if (> n 0)
      (progn
        ;; 1. select two individuals
        ;; 2. possibly crossover
        ;; 3. mutate
        ;; 4. evaluate
        ;; 5. insert
        ;; 6. evict
        (set-timeout (lambda () (evolve-helper (- n 1) cb)) 20))
      (cb)))

(defun evolve (cb)
  (set-timeout (lambda () (evolve-helper max-evals cb)) 20))

(defun best ()
  "Display the best individual in the population."
  (alert "TODO: implement best"))

))
