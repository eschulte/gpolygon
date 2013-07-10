;;; gpolygon.lisp --- polygons of images with a steady state genetic algorithm

;; Copyright (C) Eric Schulte 2013

;; Licensed under the Gnu Public License Version 3 or later

;;; Code:
(defpackage :gpolygon
  (:use :common-lisp :hunchentoot :cl-who :parenscript :cl-fad)
  (:export :serve))
(in-package :gpolygon)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *js-string-delimiter* #\"))

(defun serve (&key (port 4242))
  (start (make-instance 'hunchentoot:easy-acceptor :port port)))


;;; Main Page
(define-easy-handler (main :uri "/") ()
  (macrolet ((link (s)
               `(htm (:a :href "#" :onclick (ps* (list ,s)) (str ,s)) " ")))
    (let ((actions '(evolve mcmc stop show-best load-polygon))
          (params '(max-length population-size tournament-size delay)))
      (with-html-output-to-string (s)
        (:html (str "<!-- Copyright (C) Eric Schulte 2013, License GPLV3 -->")
         (:head (:title "evolve polygons to match images"))
         (:body :onload (ps (setup))
          (:table (:tr (:th "target image") (:th "current "))
                  (:tr (:td (:canvas :id "target" :onclick (ps (set-image))))
                       (:td (:canvas :id "current" :onclick (ps (get-best))))))
          (:table (:tr (:th "Actions") (:td (mapc (lambda (a) (link a)) actions)))
                  (:tr (:th "Parameters") (:td (mapc (lambda (p) (link p)) params)))
                  (loop :for stat :in '("best" "mean" "evals" "length") :do
                     (htm (:tr (:th (str stat)) (:td :id stat "no js")))))
          (:script :type "text/javascript" (str (ps


;;; JavaScript
(defmacro randomly (&rest body)
  (let ((rnd (ps-gensym "rand")) (lng (length body)) (cnt 0))
    `(let ((,rnd (random)))
       (cond ,@(mapcar (lambda (f) (incf cnt) `((< ,rnd ,(/ cnt lng)) ,@f))
                       (butlast body))
             (t ,@(car (last body)))))))

(defvar img (new (-image))) (defvar width nil) (defvar height nil)

(defun setup ()
  (stats) (set-interval stats 1000)
  (flet ((write (id text)
           (let ((x (chain document (get-element-by-id id) (get-context "2d"))))
             (setf (@ x fill-style) "black" (@ x font) "12pt Arial")
             (chain x (fill-text text 80 60)))))
    (let ((image (chain (regex "image=([^&#]*)") (exec (@ location search))))
          (poly (chain (regex "polygon=([^&#]*)") (exec (@ location search)))))
      (if (null image)
          (write "target" "click to set image")
          (set-image (decode-u-r-i-component (aref image 1))))
      (if (null poly)
          (write "current" "click to get polygons")
          (load-polygon (decode-u-r-i-component (aref poly 1)))))))

;; Image server must have enabled CORS -- http://enable-cors.org/
(defun set-image (&optional url cb)
  (let ((canvas (chain document (get-element-by-id "target")))
        (current (chain document (get-element-by-id "current"))))
    (setf img (new (-image))
          (@ img onload)
          (lambda ()
            (setf
             (@ canvas width) (@ img width) (@ canvas height) (@ img height)
             (@ current width) (@ img width) (@ current height) (@ img height)
             width (@ img width) height (@ img height))
            (chain canvas (get-context "2d") (draw-image img 0 0))
            (when cb (funcall cb)))
          (@ img cross-origin) "anonymous" ;; w/o this display any, but no data
          (@ img src)
          (or url (prompt "enter an image url (from a CORS enabled server)"
                          "./images/mona-lisa.png")))))

(defun load-polygon (data)
  (let ((data (or data (prompt "paste in the JSON of a polygon"))))
    (unless (null data)
      (let ((individual (chain -j-s-o-n (parse data))))
        (set-image
         (@ individual :url)
         (lambda ()
           (evaluate individual)
           ;; show the individual
           (stop) (clear) (chain individual :genome (map draw))
           ;; incorporate into the population
           (chain window pop (sort fit-sort) (splice -1 1 individual))))))))

(defun clear ()
  (chain document (get-element-by-id "current") (get-context "2d")
         (clear-rect 0 0 width height)))

(defun draw-color (c)
  (+ "rgba(" (aref c 0) ", " (aref c 1) ", " (aref c 2) ", " (aref c 3) ")"))

(defun draw (poly)
  (let* ((points (chain (getprop poly :vertices) (slice 0)))
         (head (chain points pop))
         (cx (chain document (get-element-by-id "current") (get-context "2d"))))
    (setf (@ cx fill-style) (draw-color (getprop poly :color)))
    (chain cx (begin-path))
    (chain cx (move-to (@ head 0) (@ head 1)))
    (chain points (map (lambda (point)
                         (chain cx (line-to (@ point 0) (@ point 1))))))
    (chain cx (close-path))
    (chain cx (fill))))

(defun data (id)
  (try (chain document (get-element-by-id id) (get-context "2d")
              (get-image-data 0 0 width height) data)
       (:catch (e) (alert "no image data set"))))

(defun score ()
  (let* ((dc (data "current")) (dt (when dc (data "target"))))
    (loop :for i :from 0 :below (length dc) :sum
       (abs (- (getprop dc i) (getprop dt i))))))

(defun add-poly () (draw (poly)))
(defun add-ind () (evaluate (new-ind)))


;;; Individuals
(defvar evals 0)
(defvar soft-genome-length 128)
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
  (setf (getprop ind :fit)
        (* (score) (/ (max soft-genome-length (length (getprop ind :genome)))
                      soft-genome-length)))
  ind)

(defun crossover (a b) ;; one point crossover
  (let ((pt (min (length (getprop a :genome)) (length (getprop b :genome)))))
    (create fit nil genome
            (append (chain (getprop a :genome) (slice 0 pt) (map copy-poly))
                    (chain (getprop b :genome) (slice pt) (map copy-poly))))))

(defun tweak-range (n range)
  (randomly ((min (+ n (random (/ n 8))) range)) ; bigger
            ((max (- n (random (/ n 8))) 0))))   ; smaller

;; seems to work better w/o color tweaks
(defun tweak-poly (poly)
  (randomly
   ((let ((vert (random-elt (getprop poly :vertices))))
      (randomly ((setf (aref vert 0) (tweak-range (aref vert 0) width)))
                ((setf (aref vert 1) (tweak-range (aref vert 1) height))))))
   ((let ((pt (random 4)))
      (setf (aref (getprop poly :color) 4)
            (if (= pt 3) 
                (/ (tweak-range (* 100 (aref (getprop poly :color) pt)) 100)
                   100)
                (tweak-range (aref (getprop poly :color) pt) 255)))))))

(defun mutate (ind)
  (let* ((g (chain ind :genome)) (i (random-ind g)))
    (randomly ((when (> (length g) 1) (chain g (splice i 1)))) ; delete
              ((chain g (splice i 0 (poly))))                  ; insert
              ((tweak-poly (aref g i)))                        ; tweak
              ((let* ((j (random-ind g))                      ; swap
                      (cp (copy-poly (aref g i)))
                      (pc (copy-poly (aref g j))))
                 (chain g (splice i 1 pc))
                 (chain g (splice j 1 cp))))))
  ind)


;; Populations
(defvar running t)
(defvar pop (make-array))
(defvar pop-size 128)
(defvar t-size 2)
(defvar throttle 2 "Delay in milliseconds to allow display to update.")

(defun max-length ()
  (let ((new-max (prompt "soft genome length limit:" soft-genome-length)))
    (unless (null new-max)
      (setf soft-genome-length (parse-int new-max 10))
      (chain window pop (map evaluate)))))

(defun population-size ()
  (let ((old-size pop-size)
        (new-size (prompt "population size:" pop-size)))
    (unless (null new-size)
      (setf pop-size (parse-int new-size 10))
      (if (> old-size pop-size)
          (chain window pop (splice pop-size))
          (loop :for i :from 0 :below (- pop-size old-size) :do
             (chain window pop (push (evaluate (new-ind)))))))))

(defun tournament-size ()
  (let ((new-t (prompt "tournament size:" t-size)))
    (unless (null new-t) (setf t-size (parse-int new-t 10)))))

(defun delay ()
  (let ((new-d (prompt "delay between evaluations (millis):" throttle)))
    (unless (null new-d) (setf throttle (parse-int new-d 10)))))

(defun fit-sort (a b) (- (getprop a :fit) (getprop b :fit)))
(defun mean (l) (/ (loop :for el :in l :sum el) (length l)))
(defun mean-length ()
  (mean (chain window pop (map (lambda (it) (length (getprop it :genome)))))))

(defun tournament ()
  (chain (loop :for i :from 1 :to t-size
            :collect (random-elt (chain window pop)))
         (sort fit-sort) 0))

(defun pop-helper (n)
  (if (and running (> n 0))
      (progn (chain window pop (push (evaluate (new-ind))))
             (set-timeout (lambda () (pop-helper (- n 1))) throttle))
      (set-timeout evolve-helper throttle)))

(defun evolve-helper ()
  (when running
    (chain window pop (sort fit-sort)
           (splice -1 1 (evaluate
                         (mutate (randomly
                                  ((copy-ind (tournament)))
                                  ((crossover (tournament) (tournament))))))))
    (set-timeout evolve-helper throttle)))
(defun evolve ()
  (setf running t)
  (if (> pop-size (length (chain window pop)))
      ;; populate, then evolve
      (set-timeout (lambda () (pop-helper (- pop-size (length (chain window pop)))))
                   throttle)
      ;; just evolve
      (set-timeout evolve-helper throttle)))

(defun mcmc-helper ()
  (when running
    (let* ((old-fit (chain window pop 0 :fit))
           (new-ind (evaluate (mutate (copy-ind (chain window pop 0)))))
           (new-fit (@ new-ind :fit)))
      (when (or (<  new-fit old-fit)
                (and (< (random) (/ old-fit new-fit))
                     (or (and (or (null (chain window pop 1))
                                  (< old-fit (chain window pop 1 :fit)))
                              (setf (chain window pop 1) (chain window pop 0)))
                         true)))
        (setf (chain window pop 0) new-ind))
      (set-timeout mcmc-helper throttle))))
(defun mcmc ()
  (when (null (chain window pop 0))
    (setf (chain window pop 0) (evaluate (new-ind))))
  (chain window pop (sort fit-sort) (splice 1)) (setf pop-size 1)
  (setf running t) (mcmc-helper))

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
  (let ((best (best)) (pre "data:text/JSON;base64,"))
    (loop :for (key val) :in ([] (:url img.src) (:evals evals)
                                 (:tournament-size t-size)
                                 (:population-size pop-size)
                                 (:max-length soft-genome-length))
       :do (setf (getprop best key) val))
    (chain window (open (+ pre (btoa (chain -j-s-o-n (stringify best))))))))
)))))))))


;;; Serve local images
(defun serve-img (path stream)
  (setf (content-type*) "image/png")
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (cl-fad:copy-stream in stream)))

(walk-directory "images/"
  (lambda (f) (let ((sym (intern (string-upcase (pathname-name f))))
               (uri (format nil "/images/~a" (file-namestring f))))
           (eval `(define-easy-handler (,sym :uri ,uri) ()
                    (serve-img ,f (send-headers)))))))
