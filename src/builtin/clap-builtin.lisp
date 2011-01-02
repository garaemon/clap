
(defpackage :clap-builtin
  (:use #:common-lisp)
  (:shadow #:find #:count #:set #:replace #:values)
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
           #:count
           #:endswith
           #:expandtabs
           #:find
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
           #:replace
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
           ;;#:update
           ;;#:difference-update
           ;;#:pop
           ;; dict.lisp
           #:has-key #:keys #:values #:fromkeys
           ;; meta.lisp
           #:define-class-method #:define-class-method-wrapper
           ;; condition.lisp
           #:not-implemented-yet
           #:class-method-not-supported
           #:value-error)
  )
