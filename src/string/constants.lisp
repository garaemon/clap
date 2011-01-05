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
            clap-builtin:+whitespace-characters+))

(defconstant* +letters+
  #.(concatenate 'string +lowercase+ +uppercase+))

(defconstant* +printable+
  #.(concatenate 'string
                 +digits+
                 +letters+
                 +punctuation+
                 +whitespace+))

