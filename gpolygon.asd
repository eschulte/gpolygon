(defsystem :gpolygon
  :description "gpolygon"
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on (alexandria
               metabang-bind
               curry-compose-reader-macros
               hunchentoot
               cl-who
               parenscript
               cl-fad)
  :components
  ((:file "src/package")
   (:file "src/gpolygon" :depends-on ("src/package"))))
