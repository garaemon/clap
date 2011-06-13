(in-package :clap-argparse)

(clap-builtin::defconstant* +argument-actions+
  '(:store :store-const
    :store-true :store-false :store-t :store-nil
    :append :append-const
    :version))

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
  ((name :initarg :name :initform nil
         :accessor name)
   (flags :initarg :flags :initform nil
          :accessor flags)
   (action :initarg :action :initform nil
           :accessor action)
   (nargs :initarg :nargs :initform 1
          :accessor nargs)
   (const :initarg :const :initform nil
          :accessor const)
   (default :initarg :default :initform nil
            :accessor default)
   (type :initarg :type :initform nil
         :accessor type)
   (choices :initarg :choices :initform nil
            :accessor choices)
   (required :initarg :required :initform nil
             :accessor required)
   (help :initarg :help :initform nil
         :accessor help)
   (metavar :initarg :metavar :initform nil
            :accessor metavar)
   (dest :initarg :dest :initform nil
         :accessor dest)
   (value :initarg :value :initform nil
           :accessor value)))

(defmethod print-object ((object argument) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A [~s] [~s]" (action object)
            (or (name object) (flags object)) (value object))))

(defgeneric add-argument (parser name-or-flags
                          &key
                          action nargs const
                          default type choices 
                          required help metavar dest)
  (:documentation
   "this is an implementation of ArgumentParser.add_argument"))

(defmethod add-argument ((parser argument-parser) name-or-flags
                         &key
                         (action :store) (nargs 1) (const nil)
                         (default nil) (type nil) (choices nil)
                         (required nil) (help nil) (metavar nil) (dest nil))
  (let ((arg (make-argument parser name-or-flags
                            :action action :nargs nargs :const const
                            :default default :type type :choices choices
                            :required required :help help :metavar metavar
                            :dest dest)))
    ;; check duplication
    (let ((names-and-flags
           (mapcan #'(lambda (x) (if (name x) (name x) (flags x)))
                   (arguments parser))))
      (dolist (name-or-flag name-or-flags)
        (if (find name-or-flag names-and-flags :test #'string=)
            (error "you have already use ~A option" name-or-flag))))
    ;; dest estimation
    (if (null dest)
        (if (name arg)
            (setf (dest arg) (read-from-string (name arg)))
            (setf (dest arg)
                  (read-from-string (clap-builtin:lstrip
                                     (car (flags arg))
                                     (prefix-chars parser))))))
    (push arg (arguments parser))))

(defgeneric make-argument (parser name-or-flags &rest args)
  (:documentation ""))

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

(defgeneric find-match-argument (parser arg)
  (:documentation ""))

(defmethod find-match-argument ((parser argument-parser) arg)
  (dolist (argument (arguments parser))
    (if (name argument)
        (if (string= (name argument) arg)
            (return-from find-match-argument argument))
        (dolist (flag (flags argument))
          ;; TODO: support --HOGE=hoge style
          (if (string= flag arg)
              (return-from find-match-argument argument))))))

(defgeneric action-argument (argument args)
  (:documentation "this method will process `args' according to the
`action' of `argument'. the supported `action' are :store, :store-const,
:store-true, :store-false, :append, :append-const, :version and lambda form."))

(defmethod action-argument ((argument argument) args)
  (case (action argument)
    (:store
     (if (= (nargs argument) 1)
         (setf (value argument) (car args))
         (setf (value argument) args)))
    (:store-const
     )
    ((:store-true :store-t)
     )
    ((:store-false :store-nil)
     )
    (:append
     )
    (:append-const
     )
    (:version
     )))

(defgeneric process-argument (parser argument target-arg rest-args)
  (:documentation "this method will call `action-argument' and return
the arguments which should be processed afterwards."))

(defmethod process-argument ((parser argument-parser) (argument argument)
                             target-arg rest-args)
  ;; TODO: 'name' argument is not supported yet
  (with-slots (flags nargs) argument
    (let ((next-rest-args (subseq rest-args nargs)))
      (action-argument argument (subseq rest-args 0 nargs))
      next-rest-args)))

(defgeneric make-class-from-options (parser)
  (:documentation "return a class, that is an instance of standard-class.
ths slots and thir values are defined by the `arguments' of parser."))

(defmethod make-class-from-options ((parser argument-parser))
  (let ((dests (mapcar #'dest (arguments parser))))
    (let ((anon-class
           (make-instance 'standard-class
                          :direct-slots
                          (mapcar #'(lambda (x) (list :name x
                                                      :initform x))
                                  dests))))
      (make-instance anon-class))))

(defgeneric parsed-options (parser)
  (:documentation "inner function of parse-args. this function will return
multiple values of a anonymous class instance to represent options and the list
of remained arguments."))

(defmethod parsed-options ((parser argument-parser))
  (let ((class (make-class-from-options parser)))
    (dolist (arg (arguments parser))
      (setf (slot-value class (dest arg)) (value arg)))
    class))

(defgeneric parse-args-rec (parser args &key namespace nonmatched-args)
  (:documentation "this is a helper method of parse-args. this function will
parser `args' and store the result into `arguments' of `parser'
and `nonmatched-args'. finally, it will return multiple values of options
and the remaining arguments."))

(defmethod parse-args-rec ((parser argument-parser) args
                           &key (namespace nil) (nonmatched-args nil))
  (if (null args)
      (values (parsed-options parser) (reverse nonmatched-args))
      (let ((target-arg (car args))
            (rest-args (cdr args)))
        (let ((match-argument (find-match-argument parser target-arg)))
          (if match-argument
              (parse-args-rec parser
                              (process-argument parser match-argument
                                                target-arg rest-args)
                              :namespace namespace
                              :nonmatched-args nonmatched-args)
              (parse-args-rec
               parser rest-args
               :namespace namespace
               :nonmatched-args (cons target-arg nonmatched-args)))))))

(defgeneric parse-args (parser args &key namespace)
  (:documentation "this is an implementation of ArgumentParser.parse_args. "))

(defmethod parse-args ((parser argument-parser) args &key (namespace nil))
  (parse-args-rec parser args :namespace namespace :nonmatched-args nil))
  
#|
(progn
  (require :clap-argparse)
  (setq p (make-instance 'clap-argparse:argument-parser))
  (clap-argparse:add-argument p '("-p" "-q"))
  (clap-argparse:add-argument p '("-a"))
  (clap-argparse::parse-args p (clap-builtin:split "-q 1 -a 2"))
  (pprint (clap-argparse::arguments p)))
|#
