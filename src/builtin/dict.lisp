#|
in CLAP, we does not make the special class dictionary of Python,
we just use hash table as a class of dictionary.
|#

(in-package :clap-builtin)

;; fromkeys classmethod
(define-class-method-wrapper fromkeys ((dict hash-table) keys
                                       &key initial-value test)
  "this is an implementation of dict.fromkeys.

return a new hash-table of which the keys equals to KEYS. you can
use INITIAL-VALUE to specify the values of the list.")

(define-class-method fromkeys ((dict hash-table) keys
                               &key (initial-value nil)
                                    (test #'eql))
  (let ((dict (make-hash-table :test test)))
    (loop
       for key in keys
       do (setf (gethash key dict) initial-value))
    dict))

(defgeneric has-key (dict key)
  (:documentation
   "this is an implementation of dict.has_key.

return non-NIL value if DICT has KEY in its key."))

(defmethod has-key ((dict hash-table) key)
  (multiple-value-bind (value foundp)
      (gethash key dict)
    foundp))

(defgeneric keys (dict)
  (:documentation
   "this is an implementation of dict.keys.

return a list with the keys of DICT."))

(defmethod keys ((dict hash-table))
  (maphash #'(lambda (key value)
               key)
           dict))

(defgeneric values (dict)
  (:documentation
   "this is an implementation of dict.values.

return a list with the values of DICT."))

(defmethod values ((dict hash-table))
  (maphash #'(lambda (key value)
               value)
           dict))

(defgeneric lookup (dict key)
  (:documentation
   "this is an implementation of dict[key].

return the item of DICT with the key KEY."))

(defmethod lookup ((dict hash-table) key)
  (gethash key dict))

(defgeneric (setf lookup) (val dict key)
  (:documentation
   "this is an implementation of dict[key] = val.

set the item of DICT with the key KEY to VAL."))

(defmethod (setf lookup) (val (dict hash-table) key)
  (setf (gethash key dict) val))

(defgeneric setdefault (dict key &optional default)
  (:documentation
   "this is an implementation of dict.setdefault.

update DICT destructively using KEY as the key and DEFAULT as the value only if
DICT does not have KEY in its keys."))

(defmethod setdefault ((dict hash-table) key &optional default)
  (if (has-key dict key)
      (lookup dict key)
      (progn
        (setf (lookup dict key) default)
        default)))

(defgeneric clear (dict)
  (:documentation
   "this is an implementation of dict.clear.

remove the all items from DICT"))

(defmethod clear ((dict hash-table))
  (clrhash dict)
  dict)

(defmethod copy ((dict hash-table))
  (let ((ret (make-hash-table :test (hash-table-test dict))))
    (loop
       for key being the hash-keys in dict
       using (hash-value value)
       do (setf (gethash key ret) value))
    ret))

(defgeneric get (dict key &optional default)
  (:documentation
   "this is an implementation of dict.get.

return the value for the key KEY if DICT has KEY in it, else return DEFAULT.
DEFAULT defaults to NIL"))

(defmethod get ((dict hash-table) key &optional (default nil))
  (if (has-key dict key)
      (lookup dict key)
      default))

(defgeneric items (dict)
  (:documentation
   "this is an implementation of dict.items.

return a list of (key . value)."))

(defmethod items ((dict hash-table))
  (loop
     for key being the hash-keys in dict
     using (hash-value value)
     collect `(,key . ,value)))

(defgeneric pop (dict key &optional default)
  (:documentation
   "this is an implementation of dict.pop.

if DICT has the key KEY in it, return the value of KEY and remove it from
DICT. if DICT does not have KEY, return DEFAULT only if DEFAULT is
specified. if DEFAULT is not specified, report a value-error."))

(defmethod pop ((dict hash-table) key
                &optional (default nil default-specified-p))
  (cond ((has-key dict key)
         (let ((value (lookup dict key)))
           (remhash key dict)
           value))
        (default-specified-p
         default)
        (t
         (error 'key-error
                :format-control "~A does not have ~A"
                :format-arguments `(,dict ,key)))))


(defgeneric popitem (dict)
  (:documentation
   "this is an implementation of dict.popitem.

remove and return a pair of (key . value)."))

(defmethod popitem ((dict hash-table))
  (loop
     for key being the hash-keys in dict
     using (hash-value value)
     do (progn
          (remhash key dict)
          (return `(,key . ,value)))))

(defgeneric update (dict other)
  (:documentation
   "this is an implementation of dict.update.

update DICT by another dictionary OTHER. if DICT and OTHER have
the same keys, the values of DICT will be overwritten by the
values of OTHER."))

(defmethod update ((dict hash-table) (other hash-table))
  (loop
     for key being the hash-keys in other
     using (hash-value value)
     do (setf (lookup dict key) value))
  dict)

(defgeneric delete (dict key)
  (:documentation
   "this is an implementation of del dict[key].

remove the key KEY from DICT. if DICT does not have KEY,
key-error is reported."))

(defmethod delete ((dict hash-table) key)
  (if (has-key dict key)
      (progn 
        (remhash key dict)
        dict)
      (error 'key-error
             :format-control "~A does not have ~A"
             :format-arguments `(,dict ,key))))

