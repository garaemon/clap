
(defpackage :clap-builtin
  (:use #:common-lisp)
  (:shadow #:abs)
  (:documentation "fill this documentation")
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
           #:sum
           #:zip
           #:not-implemented-yet)
  )
