(require :clap-argparse)

(defun main ()
  (let ((parser (make-instance 'clap-argparse:argument-parser
                               :description "Process some integers.")))
    (clap-argparse:add-argument parser "integers"
                                :metavar "N"
                                :type :integer
                                :nargs "+"
                                :help "an integer for the accumulator")
    (clap-argparse:add-argument parser "--sum"
                                :dest 'accumulate
                                :action :store-const
                                :const #'+
                                :default #'max
                                :help
                                "sum the integers (default: find the max)")
    (let ((args (clap-argparse:parse-args parser)))
      (print (apply (clap-builtin:lookup args 'accumulate)
                    (clap-builtin:lookup args 'integers))))))
(main)

;;(sb-ext:save-lisp-and-die "sample1" :executable t :toplevel #'main)
                          
