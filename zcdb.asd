;;;; zcdb.asd

(asdf:defsystem #:zcdb
  :serial t
           :version "1.0.2"
  :description "Read and write cdb files, as specified in
  http://cr.yp.to/cdb.html"
  :author "Zach Beane <xach@xach.com>"
  :components ((:file "package")
               (:file "cdb")
               (:file "reading")
               (:file "writing")))
