;;;; zcdb.asd

(asdf:defsystem #:zcdb
  :serial t
  :version "1.0"
  :components ((:file "package")
               (:file "cdb")
               (:file "reading")
               (:file "writing")))
