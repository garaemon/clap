(defsystem clap-argparse
    :version "0.0.0"
    :license "New BSD"
    :depends-on (clap-sys clap-builtin)
    :components
    ((:module "src/argparse" :components
              ((:file "clap-argparse")
               (:file "argument-parser" :depends-on ("clap-argparse"))))))
