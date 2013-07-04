(defpackage :gpolygon
  (:use :common-lisp
        :alexandria
        :metabang-bind
        :curry-compose-reader-macros
        :hunchentoot
        :cl-who
        :parenscript
        :cl-fad)
  (:shadowing-import-from :alexandria :bind :switch :copy-stream :copy-file))
