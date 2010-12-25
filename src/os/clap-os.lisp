(defpackage :clap-os
  (:use #:common-lisp)
  (:documentation "fill this documentation")
  (:export :uname))

(in-package :clap-os)

(defun uname ()
  "(values sysname nodename release version machine)"
  (flet (#+sbcl
         (uname (arg)
           (let* ((process (sb-ext:run-program "uname"
                                               (list arg)
                                               :input nil
                                               :output :stream
                                               :search T))
                  (output (read-line (sb-impl::process-output process) nil)))
             (close (sb-impl::process-output process))
             (string-right-trim clap-string:+whitespace+
                                output)))
         #-sbcl
         (uname (arg) arg))
    (values
     ;; sysname
     (uname "-s")
     ;; nodename
     (uname "-n")
     ;; release
     (uname "-r")
     ;; version
     (uname "-v")
     ;; machine
     (uname "-m"))))
