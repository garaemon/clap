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
>>> string.capwords("foo bar baz" "") # not supported in clap, please use nil
'Foo Bar Baz'
>>> string.capwords("foo bar baz" " ")
'Foo Bar Baz'
>>> string.capwords("foo bar             baz" " ")
'Foo Bar Baz'
||#

(defun capwords (s &optional (sep " "))
  "this is an implementation of string.capwords.

capitalize the string S using the methods defined in clap-builtin.

first of all, split S using clap-builtin:split with SEP. SEP defaults to
\" \".

secondly, capitalize the each separated string using clap-builtin:capitalize.

thirdly, joint the capitalized strings using clap-builtin:join."
  (clap-builtin:join (or sep " ")
                     (remove-if #'(lambda (x)
                                    (string= x ""))
                                (mapcar #'clap-builtin:capitalize
                                        (clap-builtin:split s sep)))))

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
