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
  ((argument :initarg :argument
             :accessor invalid-choice-argument)
   (value :initarg :value
          :accessor invalid-choice-value)
   (choices :initarg :choices
            :accessor invalid-choice-choices))
  (:report
   (lambda (c s)
     (format s "argument ~A: invalid choice: ~A (choose from ~A)"
             (invalid-choice-argument c)
             (invalid-choice-value c)
             (invalid-choice-choices c))))
  (:documentation
   "a condition to be reporeted if the arguments are not included in :choices"))
