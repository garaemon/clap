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

(defgeneric maketrans (obj from to))

(defmethod maketrans ((string string) (from string) (to string))
  (let ((ans (make-string (length string))))
    (loop :for c :across string
          :for ans-pos :from 0
          :do (let ((pos (position c from)))
                (setf (schar ans ans-pos)
                      (if pos (char to pos) c)))
          :finally (return ans))))

#|(maketrans "foo bar baあz"
           +ascii-lowercase+
           +ascii-uppercase+)
;=> "FOO BAR BAあZ"

|#

(defmethod maketrans ((symbol symbol) (from string) (to string))
  (values (intern (maketrans (symbol-name symbol) from to))))

#| (maketrans 'foo-bar-baz
           +ascii-uppercase+
           +ascii-lowercase+)
;=> |foo-bar-baz|

|#
