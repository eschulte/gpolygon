(defsystem :gpolygon
  :description "Match polygons to images with a steady state genetic algorithm."
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on (hunchentoot cl-who parenscript cl-fad)
  :components ((:file "gpolygon")))
