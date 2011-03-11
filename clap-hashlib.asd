
(defsystem clap-hashlib
  :version "0.0.0"
  :license "New BSD"
  :depends-on (ironclad)
  :components
  ((:module "src/hashlib" :components ((:file "clap-hashlib")))))