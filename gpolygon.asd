(defsystem :gpolygon
  :description "gpolygon"
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on (alexandria
               metabang-bind
               curry-compose-reader-macros
               vecto
               zpng)
  :components
  ((:file "package")
   (:file "gpolygon" :depends-on ("package"))))
