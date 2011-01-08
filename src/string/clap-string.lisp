(defpackage :clap-string
  (:use #:common-lisp)
  (:import-from #:clap-builtin #:defconstant* #:sconc)
  (:shadow #:substitute)
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
           #:capwords #:maketrans
           #:template #:substitute #:safe-substitute
           #:make-template
           #:delimiter #:idpattern)
  (:documentation "this is an implementation of string module."))


