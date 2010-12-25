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

(defgeneric string-count (str sub &key start end)
  (:documentation
   "this is an implementation of str.count.

return the number of occurrences of sub in str.

we cannot use COUNT symbol to implement it, because COUNT
symbol collides with cl-users package."))

(defmethod string-count ((str string) sub &key (start 0) (end (length str)))
  "this is an implementation of str.count.

return the number of occurrences of sub in str.

 example::

   (string-count \"foobar\" \"foo\") => 1
   (string-count \"foobar\" \"foo\" :start 1) => 0"
  (let ((ss (make-string-input-stream str start end)))
    (%string-count ss sub)))

(defun %string-count (input-stream sub)
  "this is a heloper function for string-count.

TODO: this implementation is really ugly, we need to refine it ASAP."
  (labels ((read-string (stream count)
             (let ((output (make-string-output-stream)))
               (loop for ch = (read-char stream nil nil)
                  for i from 0
                  until (or (null ch)
                            (> i count))
                  do (write-char ch output))
               (get-output-stream-string output)))
           (string-stream-starts-with
               (ch input-stream sub)
             (unread-char ch input-stream) ;pushback the read character
             (let ((str (read-string input-stream
                                     (1- (length sub)))))
               (if (and (>= (length str)  (length sub))
                        (string-equal str sub :end1 (length sub)))
                   t
                   (progn           ; pushback all the read characters
                     (loop for ch across str
                        do (unread-char ch input-stream))
                     ; need to read one character to proceed stream
                     (read-char input-stream)
                     nil)))))
    (let ((ch (read-char input-stream nil nil)))
      (cond
        ((null ch) 0)
        ((string-stream-starts-with ch input-stream sub)
         (1+ (%string-count input-stream sub)))
        (t
         (%string-count input-stream sub))))))

(defgeneric startswith (str prefix &key start end)
  (:documentation
   "this is an implementation of str.startswith.

return T if string STS starts with string PREFIX."))

(defmethod startswith ((str string) prefix &key (start 0) (end (length str)))
  "this is an implementation of str.startswith.

return T if string STS starts with string PREFIX."
  (and (>= (length str)  (length prefix))
       (string-equal str prefix
                     :end1 (min end (length prefix))
                     :start1 start)))
