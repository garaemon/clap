#|
in CLAP, we does not make the special classes for set and flozenset of Python,
we just use list as a class of set.
|#

(in-package :clap-builtin)

(declaim (inline make-set))
(defun make-set (list)
  "this is an implementation of set() and frozenset().

in CLAP, we does not make the special classes for set and flozenset of Python,
we just use list as a class of set.
so MAKE-SET returns a copy of the list LIST with removing duplicates. "
  (remove-duplicates list))


