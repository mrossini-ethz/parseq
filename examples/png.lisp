(require :asdf)
(asdf:load-system :parseq)
(use-package :parseq)

;; Parsing expression grammar example for binary data: PNG file
;; format. The PNG image file is not completely interpreted, just the
;; different chunks are identified and made available as data vectors.

;; Root parsing expression
(defrule png () (and signature header-chunk (* chunk) end-chunk))

;; Magic number for PNG files. Compare to literal vector.
(defrule signature () #(137 80 78 71 13 10 26 10))

;; Each chunk contains information about how long it is. Use a length
;; variable to store that information when read.
(defrule chunk (&optional test-type) (and chunk-length chunk-type chunk-data chunk-crc)
  (:let (len 0))
  (:test (len type data crc)
    (declare (ignore len data crc))
    (if test-type
        (string= type test-type)
        (string/= type "IEND"))))

;; Data length is 4 bytes big endian. Convert and set the length
;; variable.
(defrule chunk-length () (rep 4 byte)
  (:external len)
  (:lambda (b3 b2 b1 b0)
    (setf (ldb (byte 8 24) len) b3)
    (setf (ldb (byte 8 16) len) b2)
    (setf (ldb (byte 8 8) len) b1)
    (setf (ldb (byte 8 0) len) b0)
    len))

;; Chunk type is 4 bytes ASCII.
(defrule chunk-type () (rep 4 byte) (:string))

;; Chunk data length is indicated by the length variable. Convert to
;; vector.
(defrule chunk-data () (rep len byte) (:external len) (:function #'vector))

;; Chunk CRC is 4 bytes. Convert to vector. CRC check would be
;; possible here.
(defrule chunk-crc () (rep 4 byte) (:function #'vector))

;; Header chunk is a normal chunk of type IHDR
(defrule header-chunk () (chunk "IHDR"))

;; END chunk is a normal chunk of type IEND
(defrule end-chunk () (chunk "IEND"))

;; Enable rule tracing.
(trace-rule 'png :recursive t)

;; Parse a file. Substitute your sample file here.
(with-open-file (f "image.png" :element-type '(unsigned-byte 8))
  (let ((content (make-array (file-length f))))
    (read-sequence content f)
    (parseq 'png content :parse-error t)))
