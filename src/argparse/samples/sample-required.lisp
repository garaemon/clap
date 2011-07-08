(require :clap-argparse)

(let ((parser (make-instance 'clap-argparse:argument-parser)))
  (clap-argparse:add-argument parser "--foo" :required t)
  (describe (clap-argparse:parse-args parser
                                      '("--foo" "BAR")))
  (clap-argparse:parse-args parser nil))
