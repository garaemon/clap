(require :clap-argparse)

(let ((parser (make-instance 'clap-argparse:argument-parser :prog "PROG")))
  (clap-argparse:add-argument parser "-x")
  (clap-argparse:add-argument parser "foo" :nargs "?")
  (print (clap-argparse:parse-args parser '("-x" "-1")))
  (print (clap-argparse:parse-args parser '("-x" "-1" "-5"))))

(let ((parser (make-instance 'clap-argparse:argument-parser :prog "PROG")))
  (clap-argparse:add-argument parser "-1" :dest 'one)
  (clap-argparse:add-argument parser "foo" :nargs "?")
  (print (clap-argparse:parse-args parser '("-1" "X"))))

(let ((parser (make-instance 'clap-argparse:argument-parser :prog "PROG")))
  (clap-argparse:add-argument parser "-1" :dest 'one)
  (clap-argparse:add-argument parser "foo" :nargs "?")
  (print (clap-argparse:parse-args parser '("-2"))))

;; this sample does not invoke an error. it differs from python implementation.
(let ((parser (make-instance 'clap-argparse:argument-parser :prog "PROG")))
  (clap-argparse:add-argument parser "-1" :dest 'one)
  (clap-argparse:add-argument parser "foo" :nargs "?")
  (print (clap-argparse:parse-args parser '("-1" "-1"))))

(let ((parser (make-instance 'clap-argparse:argument-parser :prog "PROG")))
  (clap-argparse:add-argument parser "-1" :dest 'one)
  (clap-argparse:add-argument parser "foo" :nargs "?")
  (print (clap-argparse:parse-args parser '("--" "-f"))))
