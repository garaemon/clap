
(defpackage :clap-builtin
  (:use #:common-lisp)
  (:documentation "this package is an implementation of __builtin__ package")
  (:export #:range
           #:all
           #:any
           #:bin
	   #:oct
	   #:hex
           #:bool
           #:callable
           #:chr
           #:divmod
           #:filter
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
           #:not-implemented-yet)
  )
