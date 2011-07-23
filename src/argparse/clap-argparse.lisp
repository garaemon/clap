(defpackage :clap-argparse
  (:use #:common-lisp)
  (:documentation "clap-argparse is an implementation of argparse module")
  (:shadow #:prog #:type)
  (:export #:argument-parser
           #:file-type
           #:add-argument
           #:parse-args
           #:namespace
           #:print-help
           #:format-help
           #:argument-type-error
           #:add-subparsers
           #:add-parser
           #:set-defaults
           #:func
           #:add-argument-group)
  )
