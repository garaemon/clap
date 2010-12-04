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

(declaim (inline bin))
(defun bin (x)
  "this is an implementation of bin function of Python on CommonLisp.

BIN converts an integer to a binary string.

 example::

   (bin 100) => \"1100100\"
   (bin -100) => \"-1100100\""
  (declare (type integer x))
  (the string (format nil "~B" x)))

(declaim (inline oct))
(defun oct (x)
  "this is an implementation of oct function of Python on CommonLisp.

OCT converts an integer to an octal string.

  example::

    (oct 100) => \"144\"
    (oct -100) => \"-144\""
  (declare (type integer x))
  (the string (format nil "~O" x)))

(declaim (inline hex))
(defun hex (x)
  "this is an implementation of hex function of Python on CommonLisp.

HEX converts an integer to a hexadecimal string.

  example::

    (hex 100) => \"64\"
    (hex -100) => \"-64\"
    (hex 255) => \"FF\"
    (hex -255) => \"-FF\""
  (declare (type integer x))
  (the string (format nil "~X" x)))

(declaim (inline bool))
(defun bool (&optional (x nil))
  "this is an implementation of bool function of Python on CommonLisp.

BOOL convert a value to T or NIL. the value is tested by standard truth
testing of CommonLisp, which is not equivalent to the behavior of Python
testing.

if X is NIL or omitted, BOOL returns NIL and otherwise returns T.

 example::

   (bool nil) => NIL
   (bool 1) => T
   (bool -1) => T
   (bool 0) => T"
  (if x t nil))

(declaim (inline callable))
(defun callable (func)
  "this is an implementation of callable function of Python on CommonLisp.

CALLABLE returns T when FUNC is funcall-able.

 example::

   (callble #'(lambda ())) => T
   (callable #'+) => T
   (callable 1) => NIL"
  (functionp func))

(declaim (inline chr))
(defun chr (char)
  "this is an implementation of chr function of Python on CommonLisp

CHR converts an integer representing character code into a character."
  (code-char char))

(declaim (inline divmod))
(defun divmod (a b)
  "this is an implementation of divmod function of Python on CommonLisp.

DIVMOD return the quatient and remainder as multiple-values
when deviding A by B.

 example::

  (divmod 10 3) => 3 1
  (divmod 10 2) => 5 0"
  (declare (type real a b))
  (values (floor (/ a b)) (mod a b)))

(declaim (inline filter))
(defun filter (func list)
  "this is an implementation of filter function of Python on CommonLisp.

FILTER returns the elements for which function returns NON-NIL.

 example::

   (filter #'evenp '(1 2 3 4 5 6)) => (2 4 6)"
  (remove-if-not func list))

(declaim (inline hash))
(defun hash (obj)
  "this is an implementation of hash function of Python on CommonLisp.

HASH returns an integer hash code for object.

 example::

   (hash \"hogehoge\") => 444792678001011825"
  (sxhash obj))

(declaim (inline int))
(defun int (x &optional (base 10))
  "this is an implementation of int function of Python on CommonLisp.

INT converts a string, X, into an integer value. you can specify the base
by the 2nd argument. it defaults to 10.

 example::

  (int \"10\") => 10
  (int \"10\" 2) => 2"
  (declare (type integer base)
           (simple-string x))
  (parse-integer x :radix base))

(declaim (inline isinstance))
(defun isinstance (obj class)
  "this is an implementation of isinstance function of Python on CommonLisp.

ISINSTANCE returns T if OBJ is an instance of CLASS or a subclass of CLASS.

 example::

   (isinstance 1 'integer) => T
   (isinstance 1 'number) => T
   (isinstance 1 'single-float) => NIL"
  (typep obj class))

(declaim (inline issubclass))
(defun issubclass (sub super)
  "this is an implementation of issubclass function of Python on CommonLisp.

ISSUBCLASS returns T if SUB is a subclass of SUPER.

 example::

  (clap-builtin:issubclass 'number 'single-float) => NIL
  (clap-builtin:issubclass  'single-float 'number) => T"
  (closer-mop:subclassp sub super))

(declaim (inline len))
(defun len (x)
  "this is an implementation of len function of Python on CommonLisp.

LEN returns the length of X.

 example::

   (len '()) => 0
   (len '(0 1 2)) => 3"
  (length x))

(defun ord (c)
  (error 'not-implemented-yet))

(defun pow (x y &optional (z nil))
  "this is an implementation of pow function of Python on CommonLisp.

POW retuerns x to the power y. if z is given, POW returns x to the power y and modulo
z."
  (declare (type number x y)
           (type (or number null) z))
  (if z
      (mod (expt x y) z)
      (expt x y)))
  

(defun enumerate (lst &optional (start 0))
  "this is an implementation of enumerate function of Python on CommonLisp.

ENUMERATE is a function to create lists containing lists whose elements
are a count (from START which defaults to 0) and the corresponding value of
LST.

note that although original enumerate of Python returns an iterator, this
version returns a list.

 example::

  (enumerate '(A B C)) => ((0 A) (1 B) (2 C))
  (enumerate '(A B C) 1) => ((1 A) (2 B) (3 C))
  (enumerate '()) => NIL
  (enumerate '() 123) => NIL"
  (declare (type list lst)
	   (type integer start))
  (loop for i from start and x in lst collect (list i x)))
