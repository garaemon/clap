
(defpackage :clap-hashlib
  (:use #:common-lisp)
  (:documentation "fill this documentation")
  (:export #:md5
	   #:sha1
	   #:sha224
	   #:sha256
	   #:sha384
	   #:sha512
	   #:update
	   #:digest
	   #:hexdigest
	   #:copy
	   #:digest-size))

(in-package :clap-hashlib)

(defclass hash-class ()
  ((digest-algorithm :reader digest-algorithm :initarg :digest-algorithm)
   (value :accessor value-of :initform "" :initarg :value)))

(defgeneric update (hash-class string))
(defgeneric digest (hash-class))
(defgeneric hexdigest (hash-class))
(defgeneric copy (hash-class))

(defmethod digest ((hash-class hash-class))
  (ironclad:digest-sequence
   (digest-algorithm hash-class)
   (ironclad:ascii-string-to-byte-array (value-of hash-class))))

(defmethod hexdigest ((hash-class hash-class))
  (ironclad:byte-array-to-hex-string
   (digest hash-class)))
		 
(defmethod update ((hash-class hash-class) (string string))
  (setf (value-of hash-class)
	(concatenate 'string
		     (value-of hash-class)
		     string)))

(defmethod copy ((hash-class hash-class))
  (make-instance (class-of hash-class) :value (value-of hash-class)))

(defmacro def-hash-class (name algorithm)
  (let ((class-name
	 (values
	  (intern (format nil "~(hash-class-~A~)" name)
		  :clap-hashlib))))
  `(progn
     (defclass ,class-name (hash-class)
       ()
       (:default-initargs :digest-algorithm ,algorithm))
     (defun ,name (value)
       (make-instance ',class-name :value value)))))

(def-hash-class crc24 :crc24)
(def-hash-class crc32 :crc32)
(def-hash-class adler32 :adler32)
(def-hash-class md2 :md2)
(def-hash-class md4 :md4)
(def-hash-class md5 :md5)
(def-hash-class sha1 :sha1)
(def-hash-class sha224 :sha224)
(def-hash-class sha256 :sha256)
(def-hash-class sha384 :sha384)
(def-hash-class sha512 :sha512)
(def-hash-class ripemd160 :ripemd-160)
(def-hash-class ripemd128 :ripemd-128)
(def-hash-class tiger :tiger)
(def-hash-class whirlpool :whirlpool)
(def-hash-class tree-hash :tree-hash)