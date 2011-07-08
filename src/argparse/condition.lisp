(in-package :clap-argparse)

(define-condition argparse-error (simple-error)
  ()
  (:documentation
   "this is a superclass of the conditions which argument-parser
class reports."))

(define-condition too-few-arguments (argparse-error)
  ()
  (:report
   (lambda (c s)
     (format s "too few arguments")))
  (:documentation
   "this is a condition which will be reported if there are too
few arguments."))

(define-condition argument-type-error (argparse-error)
  ()
  (:documentation
   "a condition to be reporeted if there is an error cousing from type
conversion."))

(define-condition invalid-choice (argparse-error)
  ()
  (:report
   (lambda (c s)
     (format s "invalid choice")))
  (:documentation
   "a condition to be reporeted if the arguments are not included in :choices"))
