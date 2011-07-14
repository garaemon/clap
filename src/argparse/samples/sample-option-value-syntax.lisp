(require :clap-argparse)

(let ((parser (make-instance 'clap-argparse:argument-parser :prog "PROG")))
  (clap-argparse:add-argument parser "-x")
  (clap-argparse:add-argument parser "--foo")
  (describe (clap-argparse:parse-args parser (clap-builtin:split "-x X")))
  (describe (clap-argparse:parse-args parser (clap-builtin:split "--foo FOO"))))

(let ((parser (make-instance 'clap-argparse:argument-parser :prog "PROG")))
  (clap-argparse:add-argument parser "-x" :action :store-true)
  (clap-argparse:add-argument parser "-y" :action :store-true)
  (clap-argparse:add-argument parser "-z")
  (describe (clap-argparse:parse-args parser (clap-builtin:split "-xyzZ"))))
