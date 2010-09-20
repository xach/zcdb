;;;; writing.lisp

(in-package #:zcdb)

(defun write-cdb-u32 (u32 stream)
  "Write an (unsigned-byte 32) value to STREAM in little-endian order."
  (write-byte (ldb (byte 8 0) u32) stream)
  (write-byte (ldb (byte 8 8) u32) stream)
  (write-byte (ldb (byte 8 16) u32) stream)
  (write-byte (ldb (byte 8 24) u32) stream))

(defclass record-pointer ()
  ((hash-value
    :initarg :hash-value
    :accessor hash-value
    :documentation "The hash value of the record key.")
   (record-position
    :initarg :record-position
    :accessor record-position
    :documentation "The file position at which the record is stored."))
  (:default-initargs
   :hash-value 0
   :record-position 0)
  (:documentation "Every key/value record written to a CDB has a
  corresponding record pointer, which tracks the key's hash value and
  the record's position in the data file. When all records have been
  written to the file, these record pointers are organized into hash
  tables at the end of the cdb file."))

(defmethod print-object ((record-pointer record-pointer) stream)
  (print-unreadable-object (record-pointer stream :type t)
    (format stream "~8,'0X@~:D"
            (hash-value record-pointer)
            (record-position record-pointer))))

(defvar *empty-record-pointer* (make-instance 'record-pointer))


(defclass hash-table-bucket ()
  ((table-position
    :initarg :table-position
    :accessor table-position
    :documentation "The file position at which this table
    is (eventually) slotted.")
   (entries
    :initarg :entries
    :accessor entries
    :documentation "A vector of record-pointers."))
  (:default-initargs
   :table-position 0
   :entries (make-growable-vector))
  (:documentation "During construction of the CDB, record pointers are
  accumulated into one of 256 hash table buckets, depending on the low
  8 bits of the hash value of the key. At the end of record writing,
  these buckets are used to write out hash table vectors at the end of
  the file, and write pointers to the hash table vectors at the start
  of the file."))

(defgeneric entry-count (object)
  (:method ((object hash-table-bucket))
    (length (entries object))))

(defgeneric slot-count (object)
  (:method ((object hash-table-bucket))
    (* (entry-count object) 2)))

(defun bucket-hash-vector (bucket)
  "Create a hash vector for a bucket. A hash vector has 2x the entries
of the bucket, and is initialized to an empty record pointer. The high
24 bits of the hash value of a record pointer, mod the size of the
vector, is used as a starting slot, and the vector is walked (wrapping
at the end) to find the first free slot for positioning each record
pointer entry."
  (let* ((size (slot-count bucket))
         (vector (make-array size :initial-element nil)))
    (flet ((slot (record)
             (let ((index (mod (ash (hash-value record) -8) size)))
               (loop
                 (unless (aref vector index)
                   (return (setf (aref vector index) record)))
                 (setf index (mod (1+ index) size))))))
      (map nil #'slot (entries bucket)))
    (nsubstitute *empty-record-pointer* nil vector)))

(defmethod print-object ((bucket hash-table-bucket) stream)
  (print-unreadable-object (bucket stream :type t)
    (format stream "~D entr~:@P" (entry-count bucket))))


(defclass cdb-writer ()
  ((buckets
    :initarg :buckets
    :accessor buckets)
   (end-of-records-position
    :initarg :end-of-records-position
    :accessor end-of-records-position)
   (output
    :initarg :output
    :accessor output))
  (:default-initargs
   :end-of-records-position 2048
   :buckets (map-into (make-array 256)
                      (lambda () (make-instance 'hash-table-bucket)))))


(defun add-record (key value cdb-writer)
  "Add KEY and VALUE to a cdb file. KEY and VALUE should both
be (unsigned-byte 8) vectors."
  (let* ((output (output cdb-writer))
         (hash-value (cdb-hash key))
         (bucket-index (logand #xFF hash-value))
         (bucket (aref (buckets cdb-writer) bucket-index))
         (record-position (file-position output))
         (record-pointer (make-instance 'record-pointer
                                        :record-position record-position
                                        :hash-value hash-value)))
    (vector-push-extend record-pointer (entries bucket))
    (write-cdb-u32 (length key) output)
    (write-cdb-u32 (length value) output)
    (write-sequence key output)
    (write-sequence value output)
    (force-output output)
    (incf (end-of-records-position cdb-writer)
          (+ 8 (length key) (length value)))))

(defun write-bucket-hash-table (bucket stream)
  "Write BUCKET's hash table vector to STREAM."
  (map nil
       (lambda (pointer)
         (write-cdb-u32 (hash-value pointer) stream)
         (write-cdb-u32 (record-position pointer) stream))
       (bucket-hash-vector bucket)))

(defun write-hash-tables (cdb-writer)
  "Write the traililng hash tables to the end of the cdb
file. Initializes the position of the buckets in the process."
  (let ((stream (output cdb-writer)))
    (map nil
         (lambda (bucket)
           (setf (table-position bucket) (file-position stream))
           (write-bucket-hash-table bucket stream))
         (buckets cdb-writer))))

(defun write-pointers (cdb-writer)
  "Write the leading hash table pointers to the beginning of the cdb
file. Must be called after WRITE-HASH-TABLES, or the positions won't
be available."
  (let ((stream (output cdb-writer)))
    (file-position stream :start)
    (map nil
         (lambda (bucket)
           (let ((position (table-position bucket))
                 (count (slot-count bucket)))
             (when (zerop position)
               (error "Table positions not initialized correctly"))
             (write-cdb-u32 position stream)
             (write-cdb-u32 count stream)))
         (buckets cdb-writer))))

(defun finish-cdb-writer (cdb-writer)
  "Write the trailing hash tables and leading table pointers to the
cdb file."
  (write-hash-tables cdb-writer)
  (write-pointers cdb-writer)
  (force-output (output cdb-writer)))


(defvar *pointer-padding* (make-array 2048 :element-type '( unsigned-byte 8)))

(defun call-with-output-to-cdb (cdb-pathname temp-pathname fun)
  "Call FUN with one argument, a CDB-WRITER instance to which records
can be added with ADD-RECORD."
  (with-open-file (stream temp-pathname
                          :direction :output
                          :element-type '(unsigned-byte 8)
                          :if-exists :supersede)
    (let ((cdb (make-instance 'cdb-writer :output stream)))
      (write-sequence *pointer-padding* stream)
      (funcall fun cdb)
      (finish-cdb-writer cdb)))
  (values (rename-file temp-pathname cdb-pathname)))

(defmacro with-output-to-cdb ((cdb file temp-file) &body body)
  "Evaluate BODY with CDB bound to a CDB-WRITER object. The CDB in
progress is written to TEMP-FILE, and then when the CDB is
successfully written, TEMP-FILE is renamed to FILE. For atomic
operation, FILE and TEMP-FILE must be on the same filesystem."
  `(call-with-output-to-cdb ,file ,temp-file
                            (lambda (,cdb)
                              ,@body)))
