;;;; package.lisp

(defpackage #:zcdb
  (:use #:cl)
  ;; writing a CDB file
  (:export #:with-output-to-cdb
           #:add-record)
  ;; reading a cdb file
  (:export #:lookup
           #:map-cdb))

(in-package #:zcdb)

