(in-package :clap-builtin)

(defmethod as-integer-ratio ((number float))
  "this is an implementation of float.as_integer_ratio.

AS-INTEGER-RATIO returns a pair of integers as multiple-values.
it consists from numerator and denominator when represent the float value
in rational."
  (let ((r (rational number)))
    (values (numerator r)
            (denominator r))))

(defmethod is-integer ((number float))
  "this is an implementation of float.is_integer.

IS-INTEGER returns t if NUMBER is an integer value.

 example::

   (is-integer 1.0) => T
   (is-integer 1.1) => NIL"
  (multiple-value-bind
        (numerator denominator)
      (as-integer-ratio number)
    (if (= denominator 1)
        t
        nil)))

(defmethod hex ((number float))
  "this is an implementation of float.hex.

HEX formats a float value in hex format.

 example::

   (hex 1.2) => \"0x1.333334\"
   (hex 0.1) => \"0x0.199999\""
  (let ((plusp (plusp number))
        (abs-number (abs number))
        (precision (float-precision number)))
    (multiple-value-bind
          (integer rest)
        (floor abs-number)
      ;; represent rest in 16 modulo
      (let ((ss (make-string-output-stream)))
        (unless plusp
          (format ss "-"))
        (format ss "0x~X." integer)
        (let ((hex-bits (floor (/ (- (+ precision 3
                                        (- (mod precision 4))) 1) 4))))
          (dotimes (i hex-bits)                  ;???
            (multiple-value-bind
                  (%integer %rest)
                (floor (* 16.0 rest))
              (format ss "~X" %integer)
              (setq rest %rest))))
        (get-output-stream-string ss)))))

(define-class-method-wrapper fromhex ((number float) str)
  "this is an implementation of float.fromhex.

FROMHEX is a classmethod defined for float class.
FROMHEX returns a float parsing from STR. STR should represent
the float value in hex format.")

(defun parse-integer-callable-p (str)
  "this is a helper function.

PARSE-INTEGER-CALLABLE-P returns t if STR does not invoke errors
when str is called for PARSE-INTEGER.

  example::

    (fromhex 'float \"1.ap+2\") => 6.5"
  (cond ((= (length str) 0) nil)
        (t t)))

(define-class-method fromhex ((number real) str)
  "this is an implementation of float.fromhex.

this method is defined for real class, because in CommonLisp,
we have some classes to represent a float number, it means
single-float and double-float.

so we have a method for real class to return the number as 
a rational. in subclass of real class, we overload fromhex 
method to coerce the number into specified precision.

  example::

    (fromhex 'real \"1.ap+2\") => -13/2"
  (multiple-value-bind
        (plusp integer friction positive-exponentialp exponential)
      (decode-hex-float-code str)
    (let ((friction-value (if (parse-integer-callable-p integer)
                              (parse-integer integer :radix 16)
                              0))
          (exponential-value (* (if positive-exponentialp 1 -1)
                                (if (parse-integer-callable-p exponential)
                                    (parse-integer exponential :radix 10)
                                    0)))
          (sign (if plusp 1 -1)))
      (if (parse-integer-callable-p friction)
          (let ((friction-ss (make-string-input-stream friction)))
            (loop
               for ch = (read-char friction-ss nil nil)
               for i from 1
               until (null ch)
               do (let ((int-val (parse-integer
                                  (make-string 1 :initial-element ch)
                                  :radix 16)))
                    (setf friction-value (+ friction-value
                                            (* int-val (expt 16 (- i)))))))))
      (* sign (* friction-value (expt 2 exponential-value))))))

(define-class-method fromhex ((number float) str)
  "this is a specialized method for float class. in CommonLisp, 
there are two classes as the subclasses of float class:
single-float and double-float. in CLAP-BUILTIN, if you 
call with FROMHEX with 'FLOAT, FROMHEX will return a number
in single-float.

  example::

    (fromhex 'float \"1.ap+2\") => 6.5"
  (let ((rational (fromhex 'real str)))
    (float rational)))

(define-class-method fromhex ((number double-float) str)
  "this is a specialized method for double-float class.
if you call FROMHEX with 'DOUBLE-FLOAT, FROMHEX will
return a number in double-float.

  example::

    (fromhex 'double-float \"1.ap+2\") => 6.5d0"
  (let ((rational (fromhex 'real str)))
    (float rational 0.0d0)))

(defun decode-hex-float-code (str)
  "this is a heloper function for fromhex.

DECODE-HEX-FLOAT-CODE returns strings and booleans to represent a
floating-point value."
  (let ((ss (make-string-input-stream str)))
    ;; read first character, it may be #\-
    (let ((plusp (let ((ch (read-char ss)))
                   (if (char= ch #\-)
                       nil
                       (progn
                         (unread-char ch ss)
                         t)))))
      ;; read two characters and ensure to skip 0x prefix
      (let* ((ch1 (read-char ss nil nil))
             (ch2 (read-char ss nil nil)))
        (unless (and (and ch1 (char= ch1 #\0))
                     (and ch2 (char= ch2 #\x)))
          (unread-char ch2 ss)
          (unread-char ch1 ss)))
      (let ((integer                    ;read until #\.
             (let ((integer-ss (make-string-output-stream)))
               (loop for ch = (read-char ss nil nil)
                  until (or (null ch) (char= ch #\.) (char= ch #\p))
                  do (write-char ch integer-ss))
               (get-output-stream-string integer-ss))))
        (let ((friction                 ;read until #\p
               (let ((friction-ss (make-string-output-stream)))
                 (loop for ch = (read-char ss nil nil)
                    until (or (null ch) (char= ch #\p))
                    do (write-char ch friction-ss))
                 (get-output-stream-string friction-ss))))
          (let ((positive-exponentialp
                 (let ((ch (read-char ss nil nil)))
                   (cond ((null ch) t)
                         ((char= ch #\-) nil)
                         ((char= ch #\+) t)
                         (t (unread-char ch ss) t)))))
            (let ((exponential            ;read until EOF
                   (let ((exponential-ss (make-string-output-stream)))
                     (loop for ch = (read-char ss nil nil)
                        until (or (null ch) (char= ch #\p))
                        do (write-char ch exponential-ss))
                     (get-output-stream-string exponential-ss))))
              (values plusp integer friction
                      positive-exponentialp exponential))))))))
