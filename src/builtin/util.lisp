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

