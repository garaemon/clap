(in-package :clap-argparse)

(defclass namespace ()
  ((contents :initarg :contents :initform (clap-builtin:dict)
             :accessor contents
             :documentation "a placeholder of the parsed arguments.
its a dictionary.")
   (func :initarg :func :initform nil
         :accessor func
         :documentation "func will be available when a function is registered
using set-defaults."))
  (:documentation
   "this is an implementation of argparse.Namespace.

namespace object is used to put the result of parsing. you can access
contents using clap-builtin:lookup"))

(defmethod print-object ((object namespace) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A"
            (clap-builtin:join
             ", " (mapcar
                   #'(lambda (key value)
                       (format nil "~A=~S" key value))
                   (clap-builtin:keys (contents object))
                   (clap-builtin:values (contents object)))))))

(defmethod clap-builtin:lookup ((namespace namespace) key)
  (gethash key (contents namespace)))

(defmethod (setf clap-builtin:lookup) (val (namespace namespace) key)
  (setf (gethash key (contents namespace)) val))

(defmethod clap-builtin:.hook ((namespace namespace) accessor &rest args)
  (if (and (symbolp accessor)
           (clap-builtin:has-key (contents namespace) accessor))
      (clap-builtin:lookup namespace accessor)
      (apply accessor namespace args)))

(defgeneric concat-namespace (namespace1 namespace2)
  (:documentation "concatenate two namespace. the return value is the same to
`namespace1'"))

(defmethod concat-namespace (namespace1 namespace2)
  (loop for (key . value) in (clap-builtin:items (contents namespace2))
     do (setf (clap-builtin:lookup namespace1 key) value))
  (if (func namespace2) (setf (func namespace1) (func namespace2)))
  namespace1)
