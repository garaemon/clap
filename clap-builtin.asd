
(defsystem clap-builtin
  :version "0.0.0"
  :license "New BSD"
  :depends-on (closer-mop)
  :components
  ((:module "src/builtin" :components
            ((:file "clap-builtin")
             (:file "impl" :depends-on ("clap-builtin"))
             (:file "util" :depends-on ("clap-builtin"))))))
