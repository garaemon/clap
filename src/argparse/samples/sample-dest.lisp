(require :clap-argparse)

(let ((parser (make-instance 'clap-argparse:argument-parser)))
  (clap-argparse:add-argument parser "bar")
  (describe (clap-argparse:parse-args parser '("XXX"))))

(let ((parser (make-instance 'clap-argparse:argument-parser)))
  (clap-argparse:add-argument parser '("-f" "--foo-bar" "--foo"))
  (clap-argparse:add-argument parser '("-x" "-y"))
  (describe (clap-argparse:parse-args parser (clap-builtin:split "-f 1 -x 2")))
  (describe (clap-argparse:parse-args parser (clap-builtin:split "--foo 1 -y 2"))))
