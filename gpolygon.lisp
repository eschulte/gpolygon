(in-package :gpolygon)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))


;;; Simply polygons
(defclass polygon ()
  ((vertices :initarg :vertices :accessor vertices :initform nil)
   (color :initarg :color :accessor color :initform nil)))

(defmethod draw ((p polygon))
  (apply #'set-rgb-fill (color p))
  (apply #'move-to (car (vertices p)))
  (mapcar {apply #'line-to} (cdr (vertices p)))
  (close-subpath)
  (fill-path))


;;; Objects which we'll evolve
(defvar width 200 "Default image width.")
(defvar height 300 "Default image height.")

(defclass gpoly ()
  ((genome  :initarg :genome  :accessor genome  :initform nil)
   (fitness :initarg :fitness :accessor fitness :initform nil)))

(defmethod draw ((gp gpoly)) (mapc #'draw (genome gp)))

(defmethod phenome ((gp gpoly))
  (with-canvas (:width width :height height)
    (draw gp) (vecto::image vecto::*graphics-state*)))


;;; Usage
#+nil
(progn
  (defvar *p* (make-instance 'polygon :color '(0.2 0 0.2) :vertices '((20 20) (30 20) (25 40))))
  (defvar *gp* (make-instance 'gpoly :genome (list *p*)))
  (phenome *gp*)
  )
