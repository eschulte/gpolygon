(defpackage :gpolygon
  (:use :common-lisp
        :alexandria
        :hunchentoot
        :cl-who
        :parenscript
        :cl-fad)
  (:shadowing-import-from :alexandria :bind :switch :copy-stream :copy-file))
