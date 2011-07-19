(require :clap-argparse)

(let ((parser (make-instance 'clap-argparse:argument-parser)))
  (let ((subparsers (clap-argparse:add-subparsers parser)))
    (let ((checkout (clap-argparse:add-parser subparsers "checkout"
                                              :aliases '("co"))))
      (clap-argparse:add-argument checkout "foo")
      (print (clap-argparse:parse-args parser '("co" "bar"))))))
