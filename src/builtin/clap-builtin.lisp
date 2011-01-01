
(defpackage :clap-builtin
  (:use #:common-lisp)
  (:shadow #:find #:count #:set)
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
           #:string-find
           #:index
           #:isalnum
           #:isalpha
           #:isdigit
           #:islower
           #:isspace
           #:istitle
           #:isupper
           #:join
           #:ljust
           #:lower
           #:lstrip
           #:partition
           #:string-replace
           #:rfind
           #:rindex
           #:rjust
           #:rsplit
           #:rstrip
           #:split
           #:splitlines
           #:startswith
           #:strip
           #:swapcase
           #:title
           #:translate
           #:upper
           #:zfill
           ;; set.lisp
           #:make-set
           #:issubset
           #:isupperset
           #:union #:intersection       ;cl:union, cl:intersection
           #:difference                 ;cl:set-difference
           #:symmetric-difference
           #:isdisjoint
           #:copy
           #:define-class-method #:define-class-method-wrapper
           #:not-implemented-yet
           #:class-method-not-supported
           #:value-error)
  )
