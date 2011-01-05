(in-package :clap-string)

;; =============================================================================
;; string.capwords(s[, sep])
;; =============================================================================

#||
;; in Python
>>> string.capwords("foo bar^K baz")
'Foo Bar Baz'
>>> string.capwords("foo bar^K          baz")
'Foo Bar Baz'
>>> string.capwords("foo bar baz" "")
'Foo Bar Baz'
>>> string.capwords("foo bar             baz" "")
'Foo Bar Baz'
||#

(defgeneric capwords (obj &optional sep))

(defmethod capwords ((string string) &optional (sep " " sep-p))
  (let* ((sep (if (or (string= "" sep)
                      (find (character sep) +whitespace+))
                  " "
                  sep))
         (pat (ppcre:create-scanner
               (if sep-p
                   sep
                   (sconc "[" +whitespace+ "]")))))
    (reduce #'sconc
            (loop :for word :on (delete "" (ppcre:split pat string) :test #'string=)
                  :collect (string-capitalize (car word)) :into ans
                  :if (null (cdr word))
                    :return ans
                  :else
                    :collect sep :into ans))))

(defmethod capwords ((symbol symbol) &optional sep)
  (values (intern (capwords (symbol-name symbol) sep))))


;; =============================================================================
;; string.maketrans(from, to)
;; =============================================================================

(defun maketrans (from to)
  "this is an implementation of string.maketrans.

FROM and TO are strings and MAKETRANS returns a hash-table
for clap-builtin:translate.

 example:

    (clap-builtin:translate \"hogehoge\" (maketrans \"hoge\" \"fuga\"))
      => \"fugafuga\""
  (if (not (= (length from) (length to)))
      (error 'clap-builtin:value-error
             :format-control "the arguments of MAKETRANS must have the same length")
      (let ((ret (make-hash-table)))
        (loop
           for ch1 across from
           for ch2 across to
           do (setf (gethash ch1 ret) ch2))
        ret)))
