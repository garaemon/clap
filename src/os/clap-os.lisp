(defpackage :clap-os
  (:use #:common-lisp)
  (:documentation "fill this documentation")
  (:import-from :osicat-posix 
                . #0=(:uname
                      :getcwd
                      :getenv
                      :setenv

                      :getuid
                      :setegid
                      :seteuid
                      :setpgid
                      ;; setgroups
                      :setpgrp
                      :setregid
                      :setreuid
                      :setsid
                      :setuid
                      :strerror
                      :umask
                      :uname
                      :unsetenv))
  #+sbcl (:import-from :sb-posix 
                       . #1=(:putenv
                             :getsid
                             :setresuid))
  (:export . #0#)
  #+sbcl (:export . #1#))
