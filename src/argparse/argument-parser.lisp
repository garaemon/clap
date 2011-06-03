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
   (arguments :accessor arguments
              :initarg :arguments
              :initform nil
              :documentation "only for internal use. it just stores a list of \
argument instances.")
   )
  (:documentation
   "this is an implementation of argparse.ArgumentParser class."
   ))

(defclass argument ()
  ((name :initarg :name :initform nil)
   (flags :initarg :flags :initform nil)
   (action :initarg :action :initform nil)
   (nargs :initarg :nargs :initform nil)
   (const :initarg :const :initform nil)
   (default :initarg :default :initform nil)
   (type :initarg :type :initform nil)
   (choices :initarg :choices :initform nil)
   (required :initarg :required :initform nil)
   (help :initarg :help :initform nil)
   (metavar :initarg :metavar :initform nil)
   (dest :initarg :dest :initform nil)))

(defgeneric add-argument (parser name-or-flags
                          &key
                          action nargs const
                          default type choices 
                          required help metavar dest)
  (:documentation
   "this is an implementation of ArgumentParser.add_argument"))

(defmethod add-argument ((parser argument-parser) name-or-flags
                         &key
                         (action nil) (nargs nil) (const nil)
                         (default nil) (type nil) (choices nil)
                         (required nil) (help nil) (metavar nil) (dest nil))
  (push (make-argument parser name-or-flags
                       :action action :nargs nargs :const const
                       :default default :type type :choices choices
                       :required required :help help :metavar metavar
                       :dest dest)
        (arguments parser)))

(defmethod make-argument ((parser argument-parser) name-or-flags &rest args)
  ;; error check
  (let ((prefix (prefix-chars parser)))
    (cond
      ((> (length name-or-flags) 1)
       ;; all the flags should start with prefix
       (if (clap-builtin:all (mapcar #'(lambda (x)
                                         (clap-builtin:startswith x prefix))
                                     name-or-flags))
           (apply #'make-instance 'argument :flags name-or-flags args)
           (error "there is a flag which does not start with ~A in ~A"
                  prefix name-or-flags)))
      ((clap-builtin:startswith (car name-or-flags) prefix)
       (apply #'make-instance 'argument :flags name-or-flags args))
      (t
       (apply #'make-instance 'argument :name (car name-or-flags) args)))))

;; (setq p (make-instance 'clap-argparse:argument-parser))
;; (clap-argparse:add-argument p '("-p"))
