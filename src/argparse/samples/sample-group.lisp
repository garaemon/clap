(require :clap-argparse)

(let ((parser (make-instance 'clap-argparse:argument-parser
                             :prog "PROG" :add-help nil)))
  (let ((group (clap-argparse:add-argument-group parser "group")))
    (clap-argparse:add-argument group "--foo" :help "foo help")
    (clap-argparse:add-argument group "bar" :help "bar help"))
    (clap-argparse:print-help parser))

(let ((parser (make-instance 'clap-argparse:argument-parser
                             :prog "PROG" :add-help nil)))
  (let ((group1 (clap-argparse:add-argument-group parser "group1"
                                                  "group1 description")))
    (clap-argparse:add-argument group1 "foo" :help "foo help"))
  (let ((group2 (clap-argparse:add-argument-group parser "group2"
                                                  "group2 description")))
    (clap-argparse:add-argument group2 "--bar" :help "bar help"))
  (clap-argparse:print-help parser))