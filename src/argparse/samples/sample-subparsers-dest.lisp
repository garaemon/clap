(require :clap-argparse)

(let ((parser (make-instance 'clap-argparse:argument-parser)))
  (let ((subparsers (clap-argparse:add-subparsers parser :dest 'subparser-name)))
    (let ((subparser1 (clap-argparse:add-parser subparsers "1")))
      (clap-argparse:add-argument subparser1 "-x"))
    (let ((subparser2 (clap-argparse:add-parser subparsers "2")))
      (clap-argparse:add-argument subparser2 "y")))
  (print (clap-argparse:parse-args parser '("2" "frobble"))))
