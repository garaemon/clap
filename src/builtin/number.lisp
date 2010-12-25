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
