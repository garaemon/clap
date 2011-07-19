(require :clap-argparse)

(let ((parser (make-instance 'clap-argparse:argument-parser
                             :prog "PROG")))
  (clap-argparse:add-argument parser "--foo" :action :store-true
                              :help "foo help")
  (let ((subparsers (clap-argparse:add-subparsers parser
                                                  :help "sub-command help")))
    (let ((parser-a (clap-argparse:add-parser subparsers "a"
                                              :help "a help")))
      (clap-argparse:add-argument parser-a "bar" :type :int
                                  :help "bar help"))
    (let ((parser-b (clap-argparse:add-parser subparsers "b"
                                              :help "b help")))
      (clap-argparse:add-argument parser-b "--baz" :choices '("X" "Y" "Z")
                                  :help "baz help"))
    (print (clap-argparse:parse-args parser '("a" "12")))
    (print (clap-argparse:parse-args parser '("--foo" "b" "--baz" "Z")))
    ))

