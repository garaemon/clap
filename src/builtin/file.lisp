#|
in CLAP, we does not have any special class for file class of python,
we just use file-stream or stream for it.
|#

(in-package :clap-builtin)

(defgeneric flush (file)
  (:documentation
   "this is an implementation of file.flush.

flush the buffer of FILE."))

(defmethod flush ((file stream))
  (finish-output file))

(defgeneric fileno (file)
  (:documentation
   "this is an implementation of file.fileno.

return the integer of file descriptor which FILE has"))

(defmethod fileno ((file file-stream))
  #+(and sbcl (or darwin posix linux)) (sb-posix:file-descriptor file)
  #-(and sbcl (or darwin posix linux)) (error 'not-implemented-yet))

;; isatty not implemented

(defgeneric read (file &optional size)
  (:documentation
   "this is an implementation of file.read.

read at most SIZE bytes from FILE stream and return a string.
if size is negative number, nil or not specified, read until EOF."))

(defmethod read ((file stream) &optional (size))
  (let ((output (make-string-output-stream)))
    (if (or (and (numberp size) (minusp size))
            (null size))
        (loop for ch = (cl:read-char file nil nil)
           until (null ch)
           do (write-char ch output))
        (loop for ch = (cl:read-char file nil nil)
           for i from 0
           if (null ch)
           return nil
           else if (>= i size)
           do (progn (unread-char ch file)
                     (return))
           else
           do (write-char ch output)))
    (get-output-stream-string output)))

(defgeneric readline (file &optional size)
  (:documentation
   "this is an implementation of file.readline.

read and return one line from FILE. the string contains a newline at the
end of it.
if SIZE is specified, it is a max byte count and READLINE may return
an incomplete line."))

(defmethod readline ((file stream) &optional (size nil))
  (let ((output (make-string-output-stream)))
    (if (and (numberp size) (plusp size))
        (loop for ch = (cl:read-char file nil nil)
           for i from 0
           if (null ch)
           return nil
           else if (char= ch #\NewLine)
           do (progn (write-char ch output)
                     (return))
           else if (>= i size)
           do (progn (unread-char ch file)
                     (return))
           else
           do (write-char ch output))
        (loop for ch = (cl:read-char file nil nil)
           if (null ch)
           return nil
           else if (char= ch #\NewLine)
           do (progn (write-char ch output)
                     (return))
           else
           do (write-char ch output)))
    (get-output-stream-string output)))

(defgeneric readlines (file &optional sizehint)
  (:documentation
   "this is an implementation of file.readlines.

read until EOF and return a list of the lines. if SIZEHINT is specified,
read upto SIZEHINT bytes."))

(defmethod readlines ((file stream) &optional (sizehint nil))
  (if (and (numberp sizehint) (plusp sizehint))
      (loop
         for rest-size = sizehint then
           (if (> rest-size line-count)
               (decf rest-size line-count)
               0)
         for line = (readline file rest-size)
         for line-count = (length line)
         until (or (= rest-size 0) (string= line ""))
         collect line)
      (loop
         for line = (readline file)
         until (string= line "")
         collect line)))

(defgeneric seek (file offset &optional whence)
  (:documentation
   "this is an implementation of file.seek.

set the current FILE position to OFFSET.
WHENCE must be a one of :seek-cur, :seek-end or :seek-set and defaults to
:seek-set."))

(defmethod seek ((file stream) offset &optional (whence :seek-cur))
  (let ((position (case whence
                    (:seek-cur (+ (file-position file) offset))
                    (:seek-set offset)
                    (:seek-end (- (file-length file) offset)))))
    (file-position file position)
    file))

(defgeneric tell (file)
  (:documentation
   "this is an implementation of file.tell.

return the current position of FILE"))

(defmethod tell ((file stream))
  (file-position file))

(defgeneric write (file str)
  (:documentation
   "this is an implementation of file.write.

write a string STR to FILE."))

(defmethod write ((file stream) str)
  (write-string str file))

(defgeneric writelines (file sequence)
  (:documentation
   "this is an implementation of file.writelines.

write a list of strings SEQUENCE to FILE"))

(defmethod writelines ((file stream) sequence)
  (dolist (line sequence)
    (write file line))
  file)
