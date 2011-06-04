(defpackage :clap-argparse
  (:use #:common-lisp)
  (:documentation "clap-argparse is an implementation of argparse module")
  (:shadow #:prog #:type)
  (:export #:argument-parser
           #:add-argument)
  )
