#|
in CLAP, we does not make the special class dictionary of Python,
we just use associated lists as a class of dictionary.
|#

(in-package :clap-builtin)

;; fromkeys classmethod

;; has-key
(defgeneric has-key (dict key &key test)
  (:documentation
   "this is an implementation of dict.has_key.

return non-NIL value if DICT has KEY in its key."))


(defmethod has-key ((dict list) key &key (test #'eql))
  (loop for (akey . avalue) in dict
     if (funcall test akey key)
     do (return t)
     finally (return nil)))

;; keys
(defgeneric keys (dict)
  (:documentation
   "this is an implementation of dict.keys.

return a list with the keys of DICT."))

(defmethod keys ((dict list))
  (mapcar #'car dict))

;; values
;;(defgeneric values
;; setdefault
;; update...?
