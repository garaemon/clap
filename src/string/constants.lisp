(in-package :clap-string)

;; String constants
;; http://docs.python.org/release/2.4/lib/node104.html

(defconstant* +ascii-lowercase+
  "abcdefghijklmnopqrstuvwxyz"
  "a string composed of the ASCII lowercase characters.")

(defconstant* +ascii-uppercase+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "a string composed of the ASCII uppercase characters.")

(defconstant* +ascii-letters+
  #.(concatenate 'string
                 +ascii-lowercase+
                 +ascii-uppercase+)
  "a string composed of the ASCII alphabet characters.")

(defconstant* +digits+
  "0123456789"
  "a string composed of the digit characters")

(defconstant* +hexdigits+
  "0123456789abcdefABCDEF"
  "a string composed of the hexdigit characters")

(defconstant* +octdigits+
  "01234567"
  "a string composed of the octdigit characters")

;; FIXME -> locale
(defconstant* +lowercase+
  "abcdefghijklmnopqrstuvwxyz"
  "a string composed of the lowercase characters. currently locale
is not supported.")

;; FIXME -> locale
(defconstant* +uppercase+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "a string composed of the uppercase characters. currently locale
is not supported.")

;; FIXME -> locale
(defconstant* +punctuation+
  "!\"#$%&\'()*+,-./:;<=>?@[\\]^_`{|}~"
  "a string composed of the punctuation characters. currently locale
is not supported.")

;; whitespace
;; http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_w.htm#whitespace
(defconstant* +whitespace+
  #.(format nil
            "~{~C~}"
            clap-builtin:+whitespace-characters+)
  "a string composed of the whitespace characters.")

(defconstant* +letters+
  #.(concatenate 'string +lowercase+ +uppercase+)
  "a string composed of the letters. currently locale
is not considered.")

(defconstant* +printable+
  #.(concatenate 'string
                 +digits+
                 +letters+
                 +punctuation+
                 +whitespace+)
  "a string composed of the printable characters. currently locale
is not considered.")
