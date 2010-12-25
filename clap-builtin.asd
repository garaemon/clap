
(defsystem clap-builtin
  :version "0.0.0"
  :license "New BSD"
  :depends-on (closer-mop)
  :components
  ((:module "src/builtin" :components
            ((:file "clap-builtin")
             (:file "util" :depends-on ("clap-builtin"))
             (:file "condition" :depends-on ("clap-builtin"))
             (:file "meta" :depends-on ("clap-builtin" "condition" "util"))
             (:file "impl" :depends-on ("clap-builtin"
                                        "condition"
                                        "util"
                                        "meta"))
             (:file "number" :depends-on ("meta" "impl" "condition"))
             (:file "string" :depends-on ("meta" "impl" "condition"))))))
