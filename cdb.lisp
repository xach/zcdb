;;;; cdb.lisp

(in-package #:zcdb)

(defconstant +initial-hash-value+ 5381)

(defun cdb-hash (octets)
  "http://cr.yp.to/cdb/cdb.txt"
  (declare (type (simple-array (unsigned-byte 8) (*)) octets)
           (optimize speed))
  (let ((h +initial-hash-value+))
    (declare (type (unsigned-byte 32) h))
    (dotimes (i (length octets) h)
      (let ((c (aref octets i)))
        (setf h (logand #xFFFFFFFF (+ h (ash h 5))))
        (setf h (logxor h c))))))

(defun make-growable-vector (&key
                             (size 10) (element-type t))
  (make-array size :fill-pointer 0 :adjustable t :element-type element-type))
