(in-package :clap-argparse)

(defclass argument-parser ()
  ((description :accessor description
                :initarg :description
                :documentation "text to display BEFORE the help of the arguments")
   (epilog :accessor epilog
           :initarg :epilog
           :initform ""
           :documentation "text to display AFTER the help of the arguments")
   (add-help :accessor add-help
             :initarg :add-help
             :initform t
             :documentation "add -h, --help option to the parser. defaults to T")
   (argument-default :accessor argument-default
                     :initarg :argument-default
                     :initform nil
                     :documentation
                     "")
   (parents :accessor parents
            :initarg :parents
            :initform nil
            :documentation "")
   (prefix-chars :accessor prefix-chars
                 :initarg :prefix-chars
                 :initform "-"
                 :documentation "")
   (fromfile-prefix-chars :accessor fromfile-prefix-chars
                          :initarg :fromfile-prefix-chars
                          :initform nil
                          :documentation "")
   (conflict-handler :accessor conflict-handler
                     :initarg :conflict-handler
                     :initform nil
                     :documentation "")
   (prog :accessor prog
         :initarg :prog
         :initform (car clap-sys:*argv*)
         :documentation "")
   (usage :accessor usage
          :initarg :usage
          :initform :generated
          :documentation "")
   )
  )

