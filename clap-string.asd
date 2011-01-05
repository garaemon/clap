(asdf:defsystem :clap-string
  :version "0.0.0"
  :license "New BSD"
  :depends-on (:clap-builtin :cl-ppcre)
  :components ((:module "src/string"
                :components ((:file "clap-string")
                             (:file "constants" :depends-on ("clap-string"))
                             (:file "func"
                                    :depends-on ("clap-string" "constants"))
                             ))))
