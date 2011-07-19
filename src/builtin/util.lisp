(in-package #:clap-builtin)

(defmacro defconstant* (symbol value &optional doc)
  "only if SYMBOL is not bound, binds VALUE to SYMBOL"
  `(defconstant ,symbol (if (boundp ',symbol)
                            (symbol-value ',symbol)
                            ,value)
     ,@(when doc (list doc))))

;; borrow from TAO
(defun sconc (&rest strings)
  "(sconc \"a\" \"b\") -> \"ab\"
   (sconc \"123\" \"45\" \"6789\") -> \"123456789\"
   (sconc \"abc\" nil) -> \"abc\""
  (declare (optimize (safety 0) (speed 3))
           (dynamic-extent strings))
  (let ((len 0)
        (pos 0))
    (declare (fixnum len pos))
    (dolist (s strings)
      (declare (simple-string s))
      (incf len (length s)))
    (let ((result (make-string len)))
      (declare (simple-string result))
      (dolist (s strings)
        (declare (simple-string s))
        (loop :for c :across s
              :do (setf (schar result pos) c) (incf pos)))
      result)))

(defun flatten (list)
  "well-known flatten function"
  (cond ((atom list) list)
        ((listp (car list))
         (append (flatten (car list)) (flatten (cdr list))))
        (t
         (append (list (car list)) (flatten (cdr list))))))

(defgeneric .hook (obj accessor &rest args)
  (:documentation
   ".hook is the inner function of . macro.
you can customize the behavior of . by overloading this method."))

(declaim (inline .hook))
(defmethod .hook (obj accessor &rest args)
  (apply (symbol-function accessor) obj args))

(defmacro |.| (obj accessor &rest args)
  "behaves like python's . operator. for example:

 (. foo bar 1 2 3) => (bar foo 1 2 3)"
  `(.hook ,obj ',accessor ,@args))
