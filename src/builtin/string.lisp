(in-package :clap-builtin)

(defconstant* +whitespace-characters+
    '(#\Tab
      #\Linefeed
      #\Vt                      ;x0b
      #\Page                    ;x0c
      #\Return
      #\Space)
  "a list of whitespace characters")

(defun whitespacep (ch)
  (find ch +whitespace-characters+ :test #'char=))

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

(defgeneric endswith (str suffix &key start end)
  (:documentation
   "this is an implementation of str.endswith.

return T if string STS ends with string SUFFIX."))

(defmethod endswith ((str string) suffix
                     &key
                     (start (- (length str) (length suffix)))
                     (end (length str)))
  (and (>= (length str) (length suffix))
       (string-equal str suffix :start1 start :end1 end)))

(defgeneric expandtabs (str &optional tabsize)
  (:documentation
   "this is an implementation of str.expandtabs.

return a copy of STR which all the tab in STR are replaced by TABSIZE spaces."))

(defmethod expandtabs ((str string) &optional (tabsize 8))
  (let ((output (make-string-output-stream)))
    (loop for ch across str
         if (char= ch #\tab)
         do (dotimes (i tabsize) (write-char #\Space output))
         else
         do (write-char ch output))
    (get-output-stream-string output)))

(defgeneric string-find (str sub &key start end start2 end2)
  (:documentation
   "this is an implementation of str.find.

return the lowest index in the string STR where SUB is found.
if not found, return -1."))

(defmethod string-find ((str string) sub
                        &key (start 0) (end (length str))
                        (start2 0) (end2 (length sub)))
  (loop
     with sub-initial = (elt sub start2)
     for i from start to (- end end2)
     while (> (length str) i)
     for ch = (elt str i)
     if (char= ch sub-initial)
     do
       (let ((matchedp (loop
                          for ii from (1+ i)
                          for j from (1+ start2) below end2
                          for ch1 = (elt str ii)
                          for ch2 = (elt sub j)
                          if (not (char= ch1 ch2))
                          return nil
                          finally (return t))))
         (if matchedp
             (return i)))
     finally (return -1)))

(defgeneric index (str sub &key start end start2 end2)
  (:documentation
   "this is an implementation of str.index.

it behaves like STRING-FIND, however INDEX does not retun -1 if SUB is not found
in STR, but raise value-error condition."))

(defmethod index ((str string) sub
                  &key (start 0) (end (length str))
                  (start2 0) (end2 (length sub)))
  (let ((find-result (string-find str sub :start start :end end
                                  :start2 start2 :end2 end2)))
    (if (= find-result -1)
        (error 'value-error
               :format-control
               "cannot find ~s in ~s"
               :format-arguments (list sub str))
        find-result)))

(defgeneric isalnum (str)
  (:documentation
   "this is an implementation of str.isalnum.

return T if all the characters of STR are alphanumeric."))

(defmethod isalnum ((str string))
  (if (> (length str) 0)
      (loop for ch across str
         if (not (alphanumericp ch))
         return nil
         finally
           (return t))
      nil))

(defgeneric isalpha (str)
  (:documentation
   "this is an implementation of str.isalpha.

return T if all the characters of STR are alphabetic."))

(defmethod isalpha ((str string))
  (if (> (length str) 0)
      (loop for ch across str
         if (not (alpha-char-p ch))
         return nil
         finally
           (return t))
      nil))

(defgeneric isdigit (str)
  (:documentation
   "this is an implementation of str.isdigit.

return T if all the characters of STR are digit."))

(defmethod isdigit ((str string))
  (if (> (length str) 0)
      (loop for ch across str
         if (not (digit-char-p ch))
         return nil
         finally
           (return t))
      nil))

(defgeneric islower (str)
  (:documentation
   "this is an implementation of str.islower.

return T if all the characters of STR are lowercase.
only ASCII codes are supported."))

(defmethod islower ((str string))
  (loop
     with calledp = nil
     for ch across str
     if (alpha-char-p ch)
     do (progn
          (setf calledp t)
          (if (not (lower-case-p ch))
              (return nil)))
     finally
       (return calledp)))

(defgeneric isspace (str)
  (:documentation
   "this is an implementation of str.isspace.

return T if all the characters of STR are whitespace."))

(defmethod isspace ((str string))
  (if (> (length str) 0)
      (loop for ch across str
         if (not (whitespacep ch))
         return nil
         finally
           (return t))
      nil))

(defgeneric istitle (str)
  (:documentation
   "this is an implementation of str.isspace.

return T if all the characters of STR are titlecased."))

(defmethod istitle ((str string))
  (if (> (length str) 0)
      (loop
         with should-upper-case = T
         for ch across str
         if (whitespacep ch)
         do
           (setf should-upper-case t)
         else if (and should-upper-case
                      (not (upper-case-p ch)))
         return nil
         else
         do
           (setf should-upper-case nil)
         finally
           (return t))
      nil))

(defgeneric isupper (str)
  (:documentation
   "this is an implementation of str.islower.

return T if all the characters of STR are uppercase.
only ASCII codes are supported."))

(defmethod isupper ((str string))
  (loop
     with calledp = nil
     for ch across str
     if (alpha-char-p ch)
     do (progn
          (setf calledp t)
          (if (not (upper-case-p ch))
              (return nil)))
     finally
       (return calledp)))

(defgeneric join (str strings)
  (:documentation
   "this is an implementation of str.join.

return a string which is the concatenation of STRINGS
using STR as separators."))

(defmethod join ((str string) strings)
  (if (null strings)
      ""
      (let ((output (make-string-output-stream)))
        (format output (car strings))
        (dolist (concat-string (cdr strings))
          (format output str)
          (format output concat-string))
        (get-output-stream-string output))))

(defgeneric ljust (str width &optional fillchar)
  (:documentation
   "this is an implementation of str.ljust.

return a string left-justified in a string of length width.
you can specify padding character by FILLCHAR. FILLCHAR defaults
to #\space.

if width is less than the length of STR, LJUST returns STR."))

(defmethod ljust ((str string) width &optional (fillchar #\space))
  (if (> (length str) width)
      str
      (let ((output (make-string width :initial-element fillchar)))
        (loop
           for ch across str
           for i from 0
           do (setf (elt output i) ch))
        output)))

(defgeneric lower (str)
  (:documentation
   "this is an implementation of str.lower.

LOWER method return a copy of the string STR converted to lowercase."))

(defmethod lower ((str string))
  (string-downcase str))

(defgeneric lstrip (str &optional chars)
  (:documentation
   "this is an implementation of str.lstrip.

return a copy of the string STR with leading characters are removed if contained
in CHARS."))

(defmethod lstrip ((str string) &optional chars)
  (string-left-trim chars str))

(defgeneric partition (str separator)
  (:documentation
   "this is an implementation of str.partition.

return 3 multiple values. if the string STR has the separator SEPARATOR
 in itself, PARTITION will return the part before the separator,
separator itself and the part adter the separator.
if not, PARTITION will return the string STR itself, and two empty strings."))

(defmethod partition ((str string) separator)
  (let ((separator-index (string-find str separator)))
    (if (= separator-index -1)
        (values str "" "")
        (let ((first (subseq str 0 separator-index))
              (rest (subseq str (+ (length separator) separator-index))))
          (values first separator rest)))))

(defgeneric string-replace (str old new &optional count)
  (:documentation
   "this is an implementation of str.replace.

return a copy of the string STR with replacing OLD in STR by NEW."))

(defmethod string-replace ((str string) old new
                           &optional (count nil count-specified-p))
  (let ((ret (make-string-output-stream))
        (old-length (length old))
        (new-length (length new)))
    (loop
       for start-count = 0 then (+ index old-length)
       for index = (string-find str old :start start-count)
       and prev-index = (- old-length) then index
       until (= index -1)
       do (progn
            (loop
               for i from start-count below index
               do (write-char (elt str i) ret))
            (dotimes (i new-length)
              (write-char (elt new i) ret)))
       finally
         (loop                          ;copy the rest string
            for i from (+ old-length prev-index) below (length str)
            for ch = (elt str i)
            do (write-char (elt str i) ret)))
    (get-output-stream-string ret)))

(defgeneric rfind (str sub &key start end start2 end2)
  (:documentation
   "this is an implementation of str.rfind.

return the highest index in the string STR where SUB is found.
if not found, return -1."))

(defmethod rfind ((str string) sub
                  &key (start 0) (end (length str))
                  (start2 0) (end2 (length sub)))
  (loop
     with sub-final = (elt sub (1- end2))
     for i from (1- end) downto (1- (+ start end2))
     for ch = (elt str i)
     if (char= ch sub-final)
     do
       (let ((matchedp (loop
                          for ii downfrom (1- i)
                          for j from (1- (1- end2)) downto start2
                          for ch1 = (elt str ii)
                          for ch2 = (elt sub j)
                          if (not (char= ch1 ch2))
                          return nil
                          finally (return t))))
         (if matchedp
             (return (- i (1- end2)))))
       finally (return -1)))

(defgeneric rindex (str sub &key start end start2 end2)
  (:documentation
   "this is an implementation of str.rindex.

it behaves like STRING-RFIND, however RINDEX does not retun -1 if SUB is not found
in STR, but raise value-error condition."))

(defmethod rindex ((str string) sub
                  &key (start 0) (end (length str))
                  (start2 0) (end2 (length sub)))
  (let ((find-result (rfind str sub :start start :end end
                            :start2 start2 :end2 end2)))
    (if (= find-result -1)
        (error 'value-error
               :format-control
               "cannot find ~s in ~s"
               :format-arguments (list sub str))
        find-result)))

(defgeneric rjust (str width &optional fillchar)
    (:documentation
   "this is an implementation of str.rjust.

return a string right-justified in a string of length width.
you can specify padding character by FILLCHAR. FILLCHAR defaults
to #\space.

if width is less than the length of STR, RJUST returns STR."))

(defmethod rjust ((str string) width &optional (fillchar #\space))
  (if (> (length str) width)
      str
      (let ((output (make-string width :initial-element fillchar)))
        (loop
           for ch-index from (1- (length str)) downto 0
           for ch = (elt str ch-index)
           for i downfrom (1- width)
           do (setf (elt output i) ch))
        output)))

(defgeneric rsplit (str &optional separator maxsplit)
  (:documentation
   "this is an implementation of str.rsplit.

split the string STR using SEPARATOR as a delimiter and return the list.
if SEPARATOR is not specified or NIL, SEPARATOR default to whitespaces.
if MAXSPLIT is specified, at most MAXSPLIT splits are done. RSPLIT behaves like
SPLIT except for scanning from right."))

(defmethod rsplit (str
                   &optional (separator nil separator-specified-p)
                   (maxsplit nil))
  )

(defgeneric split (str &optional separator maxsplit)
  (:documentation
   "this is an implementation of str.split.

split the string STR using SEPARATOR as a delimiter and return the list.
if SEPARATOR is not specified or NIL, SEPARATOR default to whitespaces.
if MAXSPLIT is specified, at most MAXSPLIT splits are done."))

(defmethod split ((str string)
                  &optional (separator nil separator-specified-p)
                  (maxsplit nil))
  
  (let ((ret nil))
    (if  separator
         (let ((separator-length (length separator)))
           (loop
              for previous-findindex = 0
              then (+ findindex separator-length)
              for findindex = (string-find str separator)
              then (string-find str separator :start (1+ previous-findindex))
              for findcount from 1
              if (or (= findindex -1) (and maxsplit (> findcount maxsplit)))
              do (push (subseq str previous-findindex) ret)
              else
              do (push (subseq str previous-findindex findindex) ret)
              until (or (= findindex -1) (and maxsplit (> findcount maxsplit)))))
         (loop
            with previous-index = 0       ;will be setf in do-form
            with findcount = 0            ;will be setf in do-form
            for i from 0 to (1- (length str))
            for ch = (elt str i)
            for previous-whitespacep = t then whitespacep
            for whitespacep = (whitespacep ch)
            if (and maxsplit (> findcount maxsplit))
            do (progn
                 (push (subseq str previous-index) ret)
                 (return ret))
            else if (and (not (whitespacep ch)) previous-whitespacep)
            do (progn (setf previous-index i)
                      (incf findcount))
            else if (and (whitespacep ch) (not previous-whitespacep))
            do (push (subseq str previous-index i) ret)
            finally      ;the string ends with non-whitespace character
              (if (not whitespacep) 
                  (push (subseq str previous-index i) ret))))
    (nreverse ret)))

(defgeneric startswith (str prefix &key start end)
  (:documentation
   "this is an implementation of str.startswith.

return T if string STS starts with string PREFIX."))

(defmethod startswith ((str string) prefix &key (start 0) (end (length str)))
  (and (>= (length str)  (length prefix))
       (string-equal str prefix
                     :end1 (min end (length prefix))
                     :start1 start)))
