(defpackage :clap-string
  (:use #:common-lisp)
  (:import-from #:clap-builtin #:defconstant* #:sconc)
  (:export #:+ascii-letters+ #:+ascii-lowercase+ #:+ascii-uppercase+ #:+digits+
           #:+hexdigits+ #:+letters+ #:+lowercase+ #:+octdigits+ #:+printable+
           #:+punctuation+ #:+uppercase+ #:+whitespace+
           #:capwords)
  (:documentation "fill this documentation"))

(in-package :clap-string)

;; String constants
;; http://docs.python.org/release/2.4/lib/node104.html

(defconstant* +ascii-lowercase+
  "abcdefghijklmnopqrstuvwxyz")

(defconstant* +ascii-uppercase+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defconstant* +ascii-letters+
  #.(concatenate 'string
                 +ascii-lowercase+
                 +ascii-uppercase+))

(defconstant* +digits+
  "0123456789")

(defconstant* +hexdigits+
  "0123456789abcdefABCDEF")

(defconstant* +octdigits+
  "01234567")

;; FIXME -> locale
(defconstant* +lowercase+
  "abcdefghijklmnopqrstuvwxyz")

;; FIXME -> locale
(defconstant* +uppercase+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

;; FIXME -> locale
(defconstant* +punctuation+
  "!\"#$%&\'()*+,-./:;<=>?@[\\]^_`{|}~")

;; whitespace
;; http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_w.htm#whitespace
(defconstant* +whitespace+
  #.(format nil
            "~{~C~}"
            '(#\Tab
              #\Linefeed
              #\Vt                      ;x0b
              #\Page                    ;x0c
              #\Return
              #\Space)))

(defconstant* +letters+
  #.(concatenate 'string +lowercase+ +uppercase+))

(defconstant* +printable+
  #.(concatenate 'string
                 +digits+
                 +letters+
                 +punctuation+
                 +whitespace+))

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