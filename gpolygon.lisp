(in-package :gpolygon)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *js-string-delimiter* #\"))

(defun serve ()
  "Start serving up pages."
  (start (make-instance 'hunchentoot:easy-acceptor :port 4242)))

(defvar width 216 "Image width.")
(defvar height 162 "Image height.")


;;; Pages
(define-easy-handler (main :uri "/") ()
  (with-html-output-to-string (s)
    (:html
     (:head (:script :type "text/javascript" :src "/evolve.js"))
     (:body (:table (:tr (:th "target image") (:th "current "))
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
                       (:a :href "#" :onclick (ps (show-best)) "best") ", "
                       (:a :href "#" :onclick (ps (get-best)) "download") ", "
                       (:a :href "#" :onclick (ps (do-clear)) "clear")))
             (:tr (:th "best") (:td :id "best" "javascript not enabled"))
             (:tr (:th "mean") (:td :id "mean" "javascript not enabled"))
             (:tr (:th "evals") (:td :id "evals" "javascript not enabled"))
             (:tr (:th "length") (:td :id "length" "javascript not enabled")))))))

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


;;; Page elements
(setf (@ target-img src) "/eyjafjallajokull.png")
(setf (@ target-img onload)
      (lambda () (setup) (stats)
         (chain t-cnv (get-context "2d") (draw-image target-img 0 0))))

(defun setup ()
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
    (chain points (map (lambda (point)
                         (chain ctx (line-to (@ point 0) (@ point 1))))))
    (chain ctx (close-path))
    (chain ctx (fill))))

(defun data (id)
  (let ((canvas (chain document (get-element-by-id id))))
    (chain canvas (get-context "2d")
           (get-image-data 0 0 (@ canvas width) (@ canvas height)) data)))

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
(defvar max-poly-length 6)
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
(defun copy-ind (ind) (create fit nil genome (chain ind :genome (slice 0))))

(defun evaluate (ind)
  (incf evals) (clear) (chain ind :genome (map draw))
  (setf (getprop ind :fit) (score))
  ind)

(defun crossover (a b) ;; two point crossover
  (let* ((ag (chain a :genome)) (bg (chain b :genome))
         (a-rng (list (random-ind ag) (random-ind ag)))
         (b-rng (list (random-ind bg) (random-ind bg))))
    (create fit nil genome (append (chain ag (slice 0 (min a-rng)))
                                   (chain bg (slice (min b-rng) (max b-rng)))
                                   (chain ag (slice (max a-rng)))))))

(defun tweak-range (n range)
  (let ((pull (/ (random (* 100 range)) 100)))
    (mean (list n n n n pull))))

;; seems to work better w/o color tweaks
(defun tweak-poly (poly)
  (let ((vert (random-elt (getprop poly :vertices))))
    (if (> (random) 0.5)
        ;; width
        (setf (aref vert 0) (tweak-range (aref vert 0) width))
        ;; height
        (setf (aref vert 1) (tweak-range (aref vert 1) height)))))

(defun mutate (ind)
  (let ((i (random-ind (chain ind :genome))))
    (case (random-elt '(:delete :insert :tweak :random :duplicate))
      (:delete (chain ind :genome (splice i 1)))
      (:insert (chain ind :genome (splice i 0 (poly))))
      (:tweak (tweak-poly (getprop ind :genome i)))
      (:duplicate (chain ind :genome (splice (random-ind (chain ind :genome)) 0
                                             (getprop ind :genome i))))
      (:random (setf ind (new-ind)))))
  ind)


;; Populations
(defvar running t)
(defvar pop (make-array))
(defvar pop-size 512)
(defvar tournament-size 4)
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

(defun evolve-helper ()
  (when running
    (chain window pop (sort fit-sort)
           (splice -1 1 (evaluate
                         (mutate (if (= 0 (random 2))
                                     (copy-ind (tournament))
                                     (crossover (tournament) (tournament)))))))
    (set-timeout evolve-helper disp-update-delay)))
(defun evolve () (setf running t) (set-timeout evolve-helper disp-update-delay))

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


