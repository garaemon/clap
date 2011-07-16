(require :clap-argparse)

(let ((parser (make-instance 'clap-argparse:argument-parser :prog "PROG")))
  (clap-argparse:add-argument parser "-1" :dest 'one)
  (clap-argparse:add-argument parser "foo" :nargs "?")
  (print (clap-argparse:parse-args parser '("--" "-f"))))
