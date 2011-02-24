(in-package :clap-os)

(cl:defclass environ-hash-table ()
  ((table :initform (clap-builtin:dict (osicat:environment)
                                       :test #'cl:equal)
          :accessor table-of
          :initarg :table)))


(cl:defmethod clap-builtin:lookup ((env environ-hash-table) key)
  (clap-builtin:lookup (table-of env) key))

(cl:defmethod (cl:setf clap-builtin:lookup) (val (env environ-hash-table) key)
  ;; update hashtable
  (cl:setf (clap-builtin:lookup (table-of env) key) val)
  ;; update environment variable
  (putenv (cl:format cl:nil "~A=~A" key val))
  )

(cl:defmethod clap-builtin:del ((env environ-hash-table) key)
  (clap-builtin:del (table-of env) key)
  (unsetenv key))

(cl:defmethod clap-builtin:values ((env environ-hash-table))
  (clap-builtin:values (table-of env)))

(cl:defmethod clap-builtin:keys ((env environ-hash-table))
  (clap-builtin:keys (table-of env)))

(cl:defun make-environ ()
  (cl:make-instance 'environ-hash-table))

(cl:defvar *environ* (make-environ))
