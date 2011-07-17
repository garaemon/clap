(require :clap-argparse)

(let ((parser (make-instance 'clap-argparse:argument-parser
                             :prog "PROG")))
  (clap-argparse:add-argument parser "-bacon")
  (clap-argparse:add-argument parser "-badger")
  (print (clap-argparse:parse-args parser (clap-builtin:split
                                           "-bac MMM")))
  (print (clap-argparse:parse-args parser (clap-builtin:split
                                           "-bad WOOD")))
  (print (clap-argparse:parse-args parser (clap-builtin:split
                                           "-ba BA"))))
