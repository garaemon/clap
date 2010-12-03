
(defpackage :clap-builtin
  (:use #:common-lisp)
  (:shadow #:abs)
  (:documentation "fill this documentation")
  (:export #:range
           #:abs
           #:all
           #:any
           #:bin
	   #:enumerate)
  )
