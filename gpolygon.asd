(defsystem :gpolygon
  :description "gpolygon"
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on (alexandria hunchentoot cl-who parenscript cl-fad)
  :components
  ((:file "package")
   (:file "gpolygon" :depends-on ("package"))))
