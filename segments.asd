;;;; segments.asd

(asdf:defsystem #:segments
  :description "Describe segments here"
  :author "Ryan Burnside"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:nodgui #:origin #:vecto)
  :components ((:file "package")
               (:file "segments")))


