
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
             (:file "func" :depends-on ("clap-builtin"
                                        "condition"
                                        "util"
                                        "meta"))
             (:file "string" :depends-on ("meta" "func" "condition"))
             (:file "number" :depends-on ("meta" "func" "condition"
                                                 "string"))
             (:file "set" :depends-on ("meta" "func" "condition"))))))
