(defsystem clap-pwd
  :version "0.0.0"
  :license "New BSD"
  :depends-on (clap-builtin osicat cffi)
  :components
  ((:module "src/pwd" :components
            ((:file "clap-pwd")
             (:file "pwd" :depends-on ("clap-pwd"))))))
