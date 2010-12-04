
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
           #:enumerate)
  )
