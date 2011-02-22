(asdf:defsystem :clap-os
  :version "0.0.0"
  :license "New BSD"
  :depends-on (:osicat "clap-builtin" "clap-string" "osicat")
  :components ((:module "src/os"
                :components ((:file "clap-os")))))
