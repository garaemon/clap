(require :clap-argparse)

(defvar *parser* (make-instance 'clap-argparse:argument-parser
                              :description "Process some integers."))

(clap-argparse:add-argument *parser* "integers"
                            :metavar "N"
                            :type 'int
                            :nargs "+"
                            :help "an integer for the accumulator")
(clap-argparse:add-argument *parser* "--sum"
                            :dest 'accumulate
                            :action :store-const
                            :const #'+
                            :default #'max
                            :help "sum the integers (default: find the max)")

(defvar *args* (clap-argparse:parse-args *parser*))
(print (apply (slot-value *args* 'accumulate)
              (slot-value *args* 'integers)))
