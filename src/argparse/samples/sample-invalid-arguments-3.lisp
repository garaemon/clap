(require :clap-argparse)

(let ((parser (make-instance 'clap-argparse:argument-parser :prog "PROG")))
  (clap-argparse:add-argument parser "--foo" :type :int)
  (clap-argparse:add-argument parser "bar" :nargs "?")
  (print (clap-argparse:parse-args parser '("spam" "badger"))))
