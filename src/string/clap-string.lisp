(defpackage :clap-string
  (:use #:common-lisp)
  (:import-from #:clap-builtin #:defconstant* #:sconc)
  (:export #:+ascii-letters+
           #:+ascii-lowercase+
           #:+ascii-uppercase+
           #:+digits+
           #:+hexdigits+
           #:+letters+
           #:+lowercase+
           #:+octdigits+
           #:+printable+
           #:+punctuation+
           #:+uppercase+
           #:+whitespace+
           #:capwords #:maketrans)
  (:documentation "this is an implementation of string module."))


