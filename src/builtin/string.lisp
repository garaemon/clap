(in-package :clap-builtin)

(defgeneric capitalize (str)
  (:documentation
   "this is an implementation of str.capitalize.

return a copy of the specified string with capitalizing it"))

(defmethod capitalize ((str string))
  "this is an implementation of str.capitalize.

return a copy of the specified string with capitalizing it.

 example::

   (capitalize \"hoge\") => \"Hoge\""
  (string-capitalize str))

(defgeneric center (str width &optional fillchar)
  (:documentation
   "this is an implementation of str.center.

return a string of length WIDTH centered STR.
you can use the optional argument to control the characters
used for padding."))

(defmethod center ((str string) width &optional (fillchar #\ ))
  "this is an implementation of str.center.

return a string of length WIDTH centered STR.
you can use the optional argument to control the characters
used for padding.

 example::

   (center \"hoge\" 9) => \"   hoge  \"
   (center \"foo\" 11 #\a) => \"aaaafooaaaa\""
  (let ((string-length (length str)))
    (if (> string-length width)
        str
        (let ((ret (make-string width
                                :initial-element fillchar)))
          (let ((start (ceiling (/ (- width string-length) 2))))
            (replace ret str :start1 start :end1 (+ start string-length))
            ret)))))

