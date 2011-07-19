(require :clap-argparse)

(let ((parser (make-instance 'clap-argparse:argument-parser
                             :prog "PROG")))
  (let ((subparser (clap-argparse:add-subparsers
                    parser
                    :title "subcommands"
                    :description "valid subcommands"
                    :help "additional help")))
    (clap-argparse:add-parser subparser "foo")
    (clap-argparse:add-parser subparser "bar")
    (clap-argparse:parse-args parser '("--help"))
    ))
