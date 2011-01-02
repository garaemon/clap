#|
in CLAP, we does not make the special class dictionary of Python,
we just use associated lists as a class of dictionary.
|#

(in-package :clap-builtin)

;; fromkeys classmethod
(define-class-method-wrapper fromkeys ((dict list) keys &optional initvalue)
  "this is an implementation of dict.fromkeys.

return a new associated list of which the keys equals to KEYS. you can
use INITVALUE to specify the values of the list.")

(define-class-method fromkeys ((dict list) keys &optional (initvalue nil))
  (loop
     for key in keys
     collect `(,key . ,initvalue)))

(defgeneric has-key (dict key &key test)
  (:documentation
   "this is an implementation of dict.has_key.

return non-NIL value if DICT has KEY in its key."))


(defmethod has-key ((dict list) key &key (test #'eql))
  (loop for (akey . avalue) in dict
     if (funcall test akey key)
     do (return t)
     finally (return nil)))

(defgeneric keys (dict)
  (:documentation
   "this is an implementation of dict.keys.

return a list with the keys of DICT."))

(defmethod keys ((dict list))
  (mapcar #'car dict))

(defgeneric values (dict)
  (:documentation
   "this is an implementation of dict.values.

return a list with the values of DICT."))
(defmethod values ((dict list))
  (mapcar #'cdr dict))

;; setdefault

