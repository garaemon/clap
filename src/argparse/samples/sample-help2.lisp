(require :clap-argparse)

(let ((parser (make-instance 'clap-argparse:argument-parser :prog "frobble")))
  (clap-argparse:add-argument parser "bar" :nargs "?"
                              :default 42 :type :int
                              :help "the bar to %(prog)s (default: %(default)s)")
  (clap-argparse:print-help parser)
  )
