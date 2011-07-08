(require :clap-argparse)

(let ((parser (make-instance 'clap-argparse:argument-parser
                             :prog "PROG")))
  (clap-argparse:add-argument parser "foo"
                              :type :integer
                              :choices (clap-builtin:range 5 10))
  (describe (clap-argparse:parse-args parser (clap-builtin:split "7")))
  (describe (clap-argparse:parse-args parser (clap-builtin:split "11"))))
