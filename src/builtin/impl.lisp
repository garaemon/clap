(in-package #:clap-builtin)

(defun range (start &optional (stop nil) (step 1))
  "this is an implementation of range function of Python on CommonLisp.

RANGE is a function to create lists containing sequential numbers.

if you call RANGE with 1 argument, RANGE returns a list filled with integers
from 0 to (1- it) at intervals of 1.

if you call RANGE with 2 or 3 arguments, they will be taken in as START, STOP
and STEP in that order, RANGE returns a list filled with integers from START to
STOP at intervals of STEP.
STEP defaults to 1 when you call RANGE with 2 arguments.

 example::

  (range 3) => (0 1 2)
  (range 4 8) => (4 5 6 7)
  (range 0 6 2) => (0 2 4)
  (range 2 0) => NIL
  (range -4) => NIL
  (range 0 -4) => NIL
  (range 0 -4 -1) => (0 -1 -2 -3)"
  (declare (type integer start step)
           (type (or null integer) stop))
  (if (null stop)
      ;; if stop is not given, start should be a positive number
      ;; and if start is a negative number, return nil
      (loop for n from 0 to (1- start) by step collect n)
      ;; if stop is given, we need to check if step is positive or negative.
      ;; if step is negative, loop macro requires to use DOWNTO.
      ;; OTOH, if step is positive, loop macro requires to use TO.
      (if (minusp step)
          (loop for n from start downto (1+ stop) by (abs step) collect n)
          (loop for n from start to (1- stop) by step collect n))))

;; range
(declaim (inline abs))
(defun abs (x)
  "this is an implementation of abs function of Python on CommonLisp.

return the absolute valud of X.

 example::

  (abs 1) => 1
  (abs -1) => 1
  (abs -2.4) => 2.4"
  (declare (type number x))
  (abs x))

(declaim (inline all))
(defun all (list)
  "this is an implementation of all function of Python on CommonLisp.

ALL returns T when LIST does not contains NIL.
if LIST equals to NIL, ALL returns T.

 example::

  (all '(t t t NIL)) => NIL
  (all '(t t t)) => T
  (all '()) => T"
  (every #'identity list))

(declaim (inline any))
(defun any (list)
  "this is an implementation of any function of Python on CommonLisp.

ANY returns non-NIL value if LIST has one or more non-NIL value(s).

 example::

  (any '(NIL NIL 1 2 3)) => 1
  (any '(NIL NIL)) => NIL
  (any '()) => NIL"
  (some #'identity list))

(defun bin (x)
  "this is an implementation of bin function of Python on CommonLisp.

BIN converts an integer to a binary string.

 example::

   (bin 100) => \"1100100\"
   (bin -100) => \"-1100100\""
  (declare (type integer x))
  (the string (format nil "~B" x)))

(defun enumerate (lst &optional (start 0))
  "this is an implementation of enumerate function of Python on CommonLisp.

ENUMERATE is a function to create lists containing lists whose elements
are a count (from START which defaults to 0) and the corresponding value of
LST.

note that although original enumerate of Python returns a iterator, this
version returns a list.

 example::
  
  (enumerate '(A B C)) => ((0 A) (1 B) (2 C))
  (enumerate '(A B C) 1) => ((1 A) (2 B) (3 C))"
  (declare (type list lst)
	   (type integer start))
  (loop for i from start and x in lst collect (list i x)))

