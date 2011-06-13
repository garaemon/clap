(in-package :clap-sys)

(defvar *argv*
  #+allegro (system:command-line-arguments)
  #+sbcl sb-ext:*posix-argv*
  #+clisp ext:*args*
  #+ecl (si:command-args)
  #+cmu ext:*command-line-words*
  #+ccl ccl:*command-line-argument-list*
  #+lispworks system:*line-arguments-list*
  "implementation of sys.argv. The list of command line arguments.")

(defun exit (code)
  #+sbcl (sb-unix:unix-exit code))
