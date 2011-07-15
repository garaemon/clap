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

(define-condition invalid-type-error (argument-type-error)
  ((argument :initarg :argument
             :accessor invalid-type-error-argument)
   (value :initarg :value
          :accessor invalid-type-error-value)
   (type :initarg :type
         :accessor invalid-type-error-type))
  (:report
   (lambda (c s)
     (format s "argument ~A: invalid ~A value: ~A"
             (invalid-type-error-argument c)
             (invalid-type-error-type c)
             (invalid-type-error-value c))))
  (:documentation
   ""))

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

(define-condition required-option (argparse-error)
  ((option :initarg :option
           :accessor required-option-option))
  (:report
   (lambda (c s)
     (format s "option ~A is required" (required-option-option c))))
  (:documentation
   "a condition to be reported if the arguments is not specified though
:require is true"))

(define-condition unrecognized-arguments (argparse-error)
  ((args :initarg :args
         :accessor unrecognized-arguments-args))
  (:report
   (lambda (c s)
     (format s "unrecognized arguments: ~{~A~^ ~}"
             (unrecognized-arguments-args c))))
  (:documentation
   "a condition to be reported if the arguments cant be recognized"))

(define-condition n-expected-arguments (argparse-error)
  ((num :initarg :num
        :accessor n-expected-arguments-num)
   (argument :initarg :argument
             :accessor n-expected-arguments-argument))
  (:report
   (lambda (c s)
     (format s "argument ~A: expected ~A argument(s)"
             (n-expected-arguments c)
             (n-expected-arguments-argument c))))
  (:documentation
   "a condition to be reported if the arguments did not satisfy the
number of the parameters"))

