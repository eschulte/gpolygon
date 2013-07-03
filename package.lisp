(defpackage :gpolygon
  (:use :common-lisp
        :alexandria
        :metabang-bind
        :curry-compose-reader-macros
        :hunchentoot
        :cl-who
        :parenscript
        :cl-fad)
  (:shadow :bind :switch :copy-stream :copy-file))
