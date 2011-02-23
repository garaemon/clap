(defpackage :clap-pwd
  (:use #:common-lisp #:cffi)
  (:documentation "implementation of pwd module")
  (:export #:getpwuid 
           #:getpwnam
           #:getpwall
           #:passwd
           #:pw-name
           #:pw-passwd
           #:pw-uid #:pw-gid
           #:pw-gecos #:pw-dir
           #:pw-shell))
  
