;;;; reading.lisp

(in-package #:zcdb)

(defun make-octet-vector (size)
  (make-array size :element-type '(unsigned-byte 8)))

(defun read-cdb-u32 (stream)
  (logand #xFFFFFFFF
          (logior (ash (read-byte stream) 0)
                  (ash (read-byte stream) 8)
                  (ash (read-byte stream) 16)
                  (ash (read-byte stream) 24))))

(defun lookup-record-at (position key stream)
  (file-position stream position)
  (let ((key-size (read-cdb-u32 stream))
        (value-size (read-cdb-u32 stream)))
    (when (= key-size (length key))
      (let ((test-key (make-octet-vector key-size)))
        (when (/= key-size (read-sequence test-key stream))
          (error "Could not read record key of size ~D from cdb stream"
                 key-size))
        (unless (mismatch test-key key :test #'=)
          (let ((value (make-octet-vector value-size)))
            (if (= value-size (read-sequence value stream))
                value
                (error "Could not read record value of size ~D from cdb stream"
                       value-size))))))))

(defun table-slot-lookup (key hash table-position
                          initial-slot slot-count stream)
  (let ((slot initial-slot))
    (loop
      (file-position stream (+ table-position (* slot 8)))
      (let ((test-hash (read-cdb-u32 stream))
            (record-position (read-cdb-u32 stream)))
        (when (zerop record-position)
          (return))
        (when (= hash test-hash)
          (let ((value (lookup-record-at record-position key stream)))
            (when value
              (return value))
            (setf slot (mod (1+ slot) slot-count))))))))

(defun stream-lookup (key stream)
  (let* ((hash (cdb-hash key))
         (pointer-index (logand #xFF hash)))
    (file-position stream (* pointer-index 8))
    (let ((table-position (read-cdb-u32 stream))
          (slot-count (read-cdb-u32 stream)))
      (when (plusp slot-count)
        (let ((initial-slot (mod (ash hash -8) slot-count)))
          (table-slot-lookup key hash
                             table-position initial-slot slot-count stream))))))

(defun lookup (key cdb)
  (if (streamp cdb)
      (stream-lookup key cdb)
      (with-open-file (stream cdb :element-type '(unsigned-byte 8))
        (stream-lookup key stream))))

(defun stream-map-cdb (function stream)
  (labels ((map-one-slot (i)
             (file-position stream (* i 8))
             (let ((table-position (read-cdb-u32 stream))
                   (slot-count (read-cdb-u32 stream)))
               (when (plusp slot-count)
                 (map-one-table table-position slot-count))))
           (map-one-table (position count)
             (dotimes (i count)
               (file-position stream (+ position (* i 8)))
               (let ((hash (read-cdb-u32 stream))
                     (position (read-cdb-u32 stream)))
                 (declare (ignore hash))
                 (when (plusp position)
                   (map-record position)))))
           (map-record (position)
             (file-position stream position)
             (let* ((key-size (read-cdb-u32 stream))
                    (value-size (read-cdb-u32 stream))
                    (key (make-octet-vector key-size))
                    (value (make-octet-vector value-size)))
               (read-sequence key stream)
               (read-sequence value stream)
               (funcall function key value))))
    (dotimes (i 256)
      (map-one-slot i))))

(defun map-cdb (function cdb)
  (if (streamp cdb)
      (stream-map-cdb function cdb)
      (with-open-file (stream cdb :element-type '(unsigned-byte 8))
        (stream-map-cdb function stream))))
