(require :clap-argparse)

(let ((parser (make-instance 'clap-argparse:argument-parser)))
  (clap-argparse:add-argument parser "--foo")
  (clap-argparse:add-argument parser "bar")
  (describe (clap-argparse:parse-args parser
                                      '("X" "--foo" "Y")))
  (clap-argparse:print-help parser))


(let ((parser (make-instance 'clap-argparse:argument-parser)))
  (clap-argparse:add-argument parser "--foo" :metavar "YYY")
  (clap-argparse:add-argument parser "bar" :metavar "XXX")
  (describe (clap-argparse:parse-args parser
                                      '("X" "--foo" "Y")))
  (clap-argparse:print-help parser))

(let ((parser (make-instance 'clap-argparse:argument-parser
                             :prog "PROG")))
  (clap-argparse:add-argument parser "-x" :nargs 2)
  (clap-argparse:add-argument parser "--foo" :nargs 2 :metavar '("bar" "baz"))
  (clap-argparse:print-help parser))
