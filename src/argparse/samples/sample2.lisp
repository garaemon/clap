(require :clap-argparse)

;; epilog sample
(let ((parser (make-instance 'clap-argparse:argument-parser
                             :description "A foo that bars"
                             :epilog "And that's how you'd foo a bar")))
  (clap-argparse:print-help parser))

;; :add-help sample
(let ((parser (make-instance 'clap-argparse:argument-parser
                             :prog "PROG"
                             :add-help nil)))
  (clap-argparse:add-argument parser "--foo"
                              :help "foo help")
  (clap-argparse:print-help parser))

(let ((parser (make-instance 'clap-argparse:argument-parser
                             :prog "PROG"
                             :prefix-chars "+")))
  (clap-argparse:print-help parser))

(let ((parser (make-instance 'clap-argparse:argument-parser
                             :prog "PROG"
                             :prefix-chars "+")))
  (clap-argparse:add-argument parser "+f")
  (clap-argparse:add-argument parser "++bar")
  (describe (clap-argparse:parse-args parser (clap-builtin:split "+f X ++bar Y"))))
