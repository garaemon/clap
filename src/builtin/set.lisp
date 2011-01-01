#|
in CLAP, we does not make the special classes for set and flozenset of Python,
we just use list as a class of set.
|#

(in-package :clap-builtin)

(declaim (inline set))
(defun set (list &key (test #'eql))
  "this is an implementation of set() and frozenset().

in CLAP, we does not make the special classes for set and flozenset of Python,
we just use list as a class of set.
so SET returns a copy of the list LIST with removing duplicates. "
  (remove-duplicates list :test test))

(defgeneric issubset (set other &key test)
  (:documentation
   "this is an implementation of set.issubset.

return T if all the menbers of SET is included in OTHER."))

(defmethod issubset ((set list) (other list) &key (test #'eql))
  (loop
     for element in set
     if (not (find element other :test test))
     do (return nil)
     finally (return t)))

(defgeneric isupperset (set other &key test)
  (:documentation
   "this is an implementation of set.issubset.

return T if all the menbers of OTHER is included in SET."))

(defmethod isupperset ((set list) (other list) &key (test #'eql))
  ;; should we use islower instead?
  (loop
     for element in other
     if (not (find element set :test test))
     do (return nil)
     finally (return t)))

(defgeneric union (set other &key test)
  (:documentation
   "this is an implementation of set.union.

return another set including all the members of SET and OTHER."))

(defmethod union ((set list) (other list) &key (test #'eql))
  (cl:union set other :test test))

(defgeneric intersection (set other &key test)
  (:documentation
   "this is an implementation of set.intersection.

return another set with the common elements to SET and OTHER"))

(defmethod intersection ((set list) (other list) &key (test #'eql))
  (cl:intersection set other :test test))

