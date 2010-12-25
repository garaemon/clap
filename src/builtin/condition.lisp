(in-package :clap-builtin)

(define-condition not-implemented-yet (simple-error)
  ()
  (:report
   (lambda (c s)
     (format s "sorry, this is not implemented yet")))
  (:documentation
   "this is a condition signaled when an API is not implemented"))

(define-condition class-method-not-supported (simple-error)
  ((class :reader class-method-not-supported-class))
  (:report
   (lambda (c s)
     (format s "classmethod for ~A is not supported"
             (class-method-not-supported-class c))))
  (:documentation
   "this is a condition signaled when a classmethod is called
with unsupported built-in class."))

(define-condition value-error (simple-error)
  ()
  (:documentation
   "this is a condition to represent ValueError in Python"))
