(defsystem clap-sys
    :version "0.0.0"
    :license "New BSD"
    :components
    ((:module "src/sys" :components
              ((:file "clap-sys")
               (:file "impl" :depends-on ("clap-sys"))))))
