(in-package :clap-argparse)

(defclass namespace ()
  ((contents :initarg :contents :initform (clap-builtin:dict)
             :accessor contents
             :documentation "a placeholder of the parsed arguments.
its a dictionary."))
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
