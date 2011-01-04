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

(defgeneric lookup (dict key &key test)
  (:documentation
   "this is an implementation of dict[key].

return the item of DICT with the key KEY."))

(defmethod lookup ((dict list) key &key (test #'eql))
  (cdr (assoc key dict :test test)))

(defgeneric (setf lookup) (val dict key &key test)
  (:documentation
   "this is an implementation of dict[key] = val.

set the item of DICT with the key KEY to VAL."))

(defmethod (setf lookup) (val (dict list) key &key (test #'eql))
  (if (has-key dict key :test test)
      (setf (cdr (assoc key dict :test test)) val)
      (setf dict (nconc dict (list (cons key val)))))
  val)

(defgeneric setdefault (dict key &key default test)
  (:documentation
   "this is an implementation of dict.setdefault.

update DICT destructively using KEY as the key and DEFAULT as the value only if
DICT does not have KEY in its keys."))

(defmethod setdefault ((dict list) key &key default (test #'eql))
  (if (has-key dict key :test test)
      (lookup dict key :test test)
      (progn
        (setf (lookup dict key :test test) default)
        default)))
