
(defpackage :clap-builtin
  (:use #:common-lisp)
  (:documentation "this package is an implementation of __builtin__ package")
  (:export #:range
           #:all
           #:any
           #:bin
           #:bit-length
	   #:oct
	   #:hex
           #:bool
           #:callable
           #:chr
           #:divmod
           #:filter
           #:fromhex
           #:hash
           #:int
           #:isinstance
           #:issubclass
           #:len
           #:enumerate
           #:pow
           #:raw-input
           #:reversed
           #:sorted
           #:str
           #:sum
           #:zip
           ;; string.lisp
           #:capitalize
           #:center
           #:string-count
           #:endswith
           #:expandtabs
           #:startswith
           #:define-class-method #:define-class-method-wrapper
           #:not-implemented-yet)
  )
