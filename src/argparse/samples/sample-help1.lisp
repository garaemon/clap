(require :clap-argparse)

(let ((parser (make-instance 'clap-argparse:argument-parser :prog "frobble")))
  (clap-argparse:add-argument parser "--foo" :action :store-t
                              :help "foo the bars before frobbling")
  (clap-argparse:add-argument parser "bar" :nargs "+"
                              :help "one of the bars to be frobbled")
  (clap-argparse:parse-args parser '("-h"))
  )
