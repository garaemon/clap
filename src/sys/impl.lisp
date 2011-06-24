(in-package :clap-sys)

(defun argv ()
  "implementation of sys.argv. The list of command line arguments."
  #+allegro (system:command-line-arguments)
  #+sbcl sb-ext:*posix-argv*
  #+clisp ext:*args*
  #+ecl (si:command-args)
  #+cmu ext:*command-line-words*
  #+ccl ccl:*command-line-argument-list*
  #+lispworks system:*line-arguments-list*)

(defun exit (code)
  #+sbcl (sb-unix:unix-exit code))
