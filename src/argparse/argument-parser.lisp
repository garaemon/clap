(in-package :clap-argparse)

(clap-builtin::defconstant* +argument-actions+
  '(:store :store-const
    :store-true :store-false :store-t :store-nil
    :append :append-const
    :version))

(clap-builtin::defconstant* +max-help-offset+ 10)

(defclass argument-parser ()
  ((description :accessor description
                :initarg :description
                :initform ""
                :documentation "text to display BEFORE the help of
the arguments")
   (epilog :accessor epilog
           :initarg :epilog
           :initform nil
           :documentation "text to display AFTER the help of the arguments")
   (add-help :accessor add-help
             :initarg :add-help
             :initform t
             :documentation "add -h, --help option to the parser.
defaults to T")
   (argument-default :accessor argument-default
                     :initarg :argument-default
                     :initform nil
                     :documentation
                     "the default value of the arguments. defaults to nil.")
   (parents :accessor parents           ;??
            :initarg :parents
            :initform nil
            :documentation "a list of argument-parser instances.")
   (prefix-chars :accessor prefix-chars
                 :initarg :prefix-chars
                 :initform "-"
                 :documentation "the prefix characters of the optional
arguments.")
   (fromfile-prefix-chars :accessor fromfile-prefix-chars
                          :initarg :fromfile-prefix-chars
                          :initform nil
                          :documentation "the set of characters that
prefix files from which additional arguments will be read.")
   (conflict-handler :accessor conflict-handler
                     :initarg :conflict-handler
                     :initform nil
                     :documentation "specify a strategy for resolving
the conflict of options")
   (prog :accessor prog
         :initarg :prog
         :initform (car (clap-sys:argv))
         :documentation "the name of the program.
default: (car (clap-sys:argv))")
   (usage :accessor usage
          :initarg :usage
          :initform :generated
          :documentation "the string describing the program usage. it will be
generated in default.")
   (arguments :accessor arguments
              :initarg :arguments
              :initform nil
              :documentation "only for internal use. it just stores a list of
argument instances."))
  (:documentation
   "this is an implementation of argparse.ArgumentParser class.
this class is useful to parse the command line options and arguments."))

(defmethod initialize-instance :after ((parser argument-parser)
                                       &rest initargs)
  (declare (ignore initargs))
  ;; collect arguments from `parents'
  (if (parents parser)
      (setf (arguments parser)
            (append (clap-builtin:flatten (mapcar #'arguments (parents parser)))
                    (arguments parser))))
  (if (add-help parser)
      (setf (arguments parser)
            (cons (make-help-argument parser) (arguments parser))))
  parser)
                                       

(defclass argument ()
  ((name :initarg :name :initform nil
         :accessor name
         :documentation "a string to represent positional arguments.
this slow will be specified only if the instance used to parse positional
arguments. (default: to nil)")
   (flags :initarg :flags :initform nil
          :accessor flags
          :documentation "a list of optional arguments.")
   (action :initarg :action :initform nil
           :accessor action
           :documentation "`action' need to be one of `+argument-actions+'
and it specifies the behavior of this argument.")
   (nargs :initarg :nargs :initform 1
          :accessor nargs
          :documentation "the number of the parameters this argument requires.
 (default: 1)")
   (const :initarg :const :initform nil
          :accessor const
          :documentation "this slot is avialbe only if `action' is :store-const
or :append-const. the value of `const' slot will be set as the value
if the :store-const or :append-const option is specified or `narg' is \"?\".")
   (default :initarg :default :initform nil
            :accessor default
            :documentation "the default value of the option (or argument)")
   (type :initarg :type :initform nil
         :accessor type
         :documentation "convert the `argument' to the type specified
by `type'")
   (choices :initarg :choices :initform nil
            :accessor choices)
   (required :initarg :required :initform nil
             :accessor required)
   (help :initarg :help :initform nil
         :accessor help
         :documentation "help string of the argument.")
   (metavar :initarg :metavar :initform nil
            :accessor metavar
            :documentation "`metavar' will be used for usage and help to
show the parameters of the argument.")
   (dest :initarg :dest :initform nil
         :accessor dest
         :documentation "`dest' is the symbol which the values of
the argument will be sotred in.")
   (version :initarg :version :initform nil
            :accessor version
            :documentation "a string to represent the version of the program."))
  (:documentation
   "this is a class to represent an argument or an optional."))

(defclass help-argument (argument)
  ()
  (:documentation
   "this is a special class to represent -h or --help option."))

(defclass file-type ()
  ((mode :initarg :mode :initform :input
         :accessor mode
         :documentation "mode of the file system."))
  (:documentation
   "this is an implementation of argparse.FileType.
we don't support buffer size specification because it is not support
by CL open function."))

(defgeneric make-help-argument (parser)
  (:documentation
   "make a help-argument instance using `argument-parser'."))

(defmethod make-help-argument ((parser argument-parser))
  (with-slots (prefix-chars) parser
    (make-instance 'help-argument
                   :flags (list
                           (concatenate 'string prefix-chars "h")
                           (concatenate 'string prefix-chars prefix-chars
                                        "help"))
                   :nargs 0
                   :help "show this help message and exit")))

(defmethod print-object ((object argument) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A [~s]" (action object)
            (or (name object) (flags object)))))

(defgeneric ensure-dest (arg parser)
  (:documentation
   "if `dest' is null, estimate `dest' from the name of the argument and set
`dest' slot of the argument."))

(defmethod ensure-dest ((arg argument) (parser argument-parser))
  (when (null (dest arg))
    (if (name arg)
        (setf (dest arg) (read-from-string (name arg)))
        (setf (dest arg)
              (read-from-string (clap-builtin:lstrip
                                 (car (flags arg))
                                 (prefix-chars parser))))))
  arg)

(defgeneric ensure-metavar (arg parser)
  (:documentation
   "if `metavar' is null, estimate `metavar' from the name of the argument and
set `metavar' slot of the argument."))

(defmethod ensure-metavar ((arg argument) (parser argument-parser))
  (with-slots (metavar nargs name flags) arg
    (when (and (null metavar)
               (not (and (numberp nargs) (= nargs 0))))
      (with-slots (prefix-chars) parser
        (cond
          (name
           (setf metavar (clap-builtin:upper name)))
          (flags
           ;; check `arg' has a long option or not. if so, 
           ;; use it as `metavar'
           (let ((long-options
                  (remove-if-not #'(lambda (f) (long-option-p parser f))
                                 flags)))
             (if (null long-options)
                 (setf metavar (clap-builtin:upper
                                (clap-builtin:lstrip (car flags) prefix-chars)))
                 (setf metavar (clap-builtin:upper
                                (clap-builtin:lstrip (car long-options)
                                                     prefix-chars))))))))))
  arg)

(defgeneric ensure-nargs (arg)
  (:documentation
   "set `nargs' to satisfy the `action'."))

(defmethod ensure-nargs ((arg argument))
  (with-slots (action nargs) arg
    (cond
      ((eq action :store-const)
       (setf nargs 0))
      ))
  arg)

(defgeneric verificate-duplicate-arguments (parser name-or-flags)
  (:documentation
   "report an error if `name-or-flags' duplicates with the `arguments' of
`parser'."))

(defmethod verificate-duplicate-arguments ((parser argument-parser)
                                           name-or-flags)
  (let ((names-and-flags
         (clap-builtin:flatten
          (mapcar #'(lambda (x) (if (name x) (name x) (flags x)))
                  (arguments parser)))))
    (dolist (name-or-flag (if (listp name-or-flags)
                              name-or-flags
                              (list name-or-flags)))
      (if (find name-or-flag names-and-flags :test #'string=)
          (error "you have already use ~A option" name-or-flag))))
  t)

(defgeneric ensure-version (arg parser)
  (:documentation
   "replace %(prog)s in version with prog of parser"))

(defmethod ensure-version ((arg argument)
                           (parser argument-parser))
  (if (version arg)
      (setf (version arg)
            (replace-prog (version arg) (prog parser)))))

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
                         (default nil) (type nil) (choices nil) (version nil)
                         (required nil) (help nil) (metavar nil) (dest nil))
  (let ((arg (make-argument parser name-or-flags
                            :action action :nargs nargs :const const
                            :default default :type type :choices choices
                            :required required :help help :metavar metavar
                            :dest dest :version version)))
    (verificate-duplicate-arguments parser name-or-flags)
    (ensure-dest arg parser)
    (ensure-nargs arg)
    (ensure-version arg parser)
    (ensure-metavar arg parser)
    (setf (arguments parser) (append (arguments parser) (list arg)))
    arg))

(defgeneric verificate-argument-name (parser name-or-flags)
  (:documentation
   "verificate `name-or-flags' is valid for arguments. if not,
it reports an error. it the `name-or-flags' is valid, it returns
:flags or :name."))

(defmethod verificate-argument-name ((parser argument-parser)
                                     (name-or-flags list))
  (let ((prefix (prefix-chars parser)))
    (cond
      ((> (length name-or-flags) 1) ; all the flags should start with prefix
       (if (clap-builtin:all (mapcar #'(lambda (x)
                                         (clap-builtin:startswith x prefix))
                                     name-or-flags))
           :flags
           (error "there is a flag which does not start with ~A in ~A"
                  prefix name-or-flags)))
      ((clap-builtin:startswith (car name-or-flags) prefix) ;starts with "-"
       :flags)
      ((= (length name-or-flags) 1)
       :name)
      (t
       (error "more than one names are specified as argument name")))))

(defgeneric make-argument (parser name-or-flags &rest args)
  (:documentation "create an instance of argument class."))

(defmethod make-argument ((parser argument-parser) name-or-flags &rest args)
  (if (not (listp name-or-flags))      ; force to be a list
      (apply #'make-argument parser (list name-or-flags) args)
   (case (verificate-argument-name parser name-or-flags)
     (:flags (apply #'make-instance 'argument :flags name-or-flags args))
     (:name (apply #'make-instance 'argument :name (car name-or-flags) args)))))

(defgeneric optional-argument-p (parser argument)
  (:documentation "return T if `argument' is for optional argument"))

(defmethod optional-argument-p ((parser argument-parser)
                                (argument argument))
  (not (name argument)))

(defgeneric long-option-p (parser flag)
  (:documentation "return T if flag is a long option"))

(defmethod long-option-p ((parser argument-parser) flag)
  (clap-builtin:startswith
   flag
   (concatenate 'string (prefix-chars parser) (prefix-chars parser))))

(defgeneric find-match-optional-argument (parser arg parse-result)
  (:documentation "return an instance of `argument' which is matched
to parse `arg'."))

(defmethod find-match-optional-argument
    ((parser argument-parser) arg parse-result)
  (dolist (argument (optional-arguments parser))
    (dolist (flag (flags argument))
      (cond ((and (long-option-p parser flag)
                  (= (clap-builtin:find arg "=") 1))
             (multiple-value-bind (before partitioner after)
                 (clap-builtin:partition arg "=")
               (if (string= before flag)
                   (return-from find-match-optional-argument argument))))
            ((string= flag arg)
             (return-from find-match-optional-argument argument)))))
  nil)

(defgeneric convert-type (argument val)
  (:documentation
   "convert `val' to the type specified by the argument.
if the type is nil, no conversion is accompleshed."))

(defmethod convert-type ((argument argument) val)
  (with-slots (type) argument
    (cond
      ((null type) val)
      ((or (eq type :integer)
           (eq type :int)
           (eq type 'integer)
           (eq type 'int))
       (clap-builtin:int val))
      ((or (eq type :float)
           (eq type 'float))
       (coerce (read-from-string val) 'float))
      ((or (eq type :string)
           (eq type :str)
           (eq type 'string)
           (eq type 'str))
       val)
      ((or (eq type :open)
           (eq type 'open))
       (open val))
      ((typep type 'file-type)
       ;; TODO: conversion from python like mode spec: "r", "w", ...
       (open val :direction (mode type)))
      ((functionp type)                 ;support lambda
       (funcall type val))
      )))

(defgeneric action-argument (argument args parse-result)
  (:documentation "this method will process `args' according to the
`action' of `argument'. the supported `action' are :store, :store-const,
:store-true, :store-false, :append, :append-const, :version and lambda form."))

(defmethod action-argument ((argument argument) args parse-result)
  (with-slots (action nargs const version) argument
    (symbol-macrolet ((value (clap-builtin:lookup parse-result
                                                  (dest argument))))
      (case action
        (:store
         (cond
           ((numberp nargs)
            (if (= nargs 1)
                (setf value (convert-type argument (car args)))
                (setf value (mapcar #'(lambda (x) (convert-type argument x))
                                    args))))
           ((string= nargs "?")
            (setf value (convert-type argument (car args))))
           ((string= nargs "*")
            (setf value (mapcar #'(lambda (x) (convert-type argument x)) args)))
           ((string= nargs "+")
            (setf value (mapcar #'(lambda (x) (convert-type argument x))
                                args)))))
        (:store-const
         ;; TODO: what happen if narg=2 and "store_const" are used
         ;;       in Python 2.7?
         (setf value const))
        ((:store-true :store-t)
         (setf value t))
        ((:store-false :store-nil)
         (setf value nil))
        (:append
         (if (null value)
             (if (= nargs 1)
                 (setf value (convert-type argument args))
                 (setf value (list (convert-type argument args)))))
             (if (= nargs 1)
                 (setf value (append args (convert-type argument value)))
                 (setf value (append value
                                     (list (convert-type argument args))))))
        (:append-const
         (setf value (append value (list const))))
        (:version
         (write-string version)
         (terpri)
         (clap-sys:exit 0))))))

(defgeneric process-argument (parser argument parse-result target-arg rest-args)
  (:documentation "this method will call `action-argument' and return
the arguments which should be processed afterwards."))

(defmethod process-argument ((parser argument-parser) (argument argument)
                             parse-result
                             target-arg rest-args)
  ;; TODO: 'name' argument is not supported yet
  (with-slots (flags nargs) argument
    ;; TODO: support ?
    (cond
      ((numberp nargs)
       (let ((next-rest-args (subseq rest-args nargs)))
         (action-argument argument (subseq rest-args 0 nargs) parse-result)
         next-rest-args))
      ((string= nargs "?")
       (error "not implemented yet"))
      ((string= nargs "*")
       ;; until find optional argument
       (with-slots (prefix) parser
         (let ((arg-num (loop
                           for arg in rest-args
                           for i from 0
                           until (clap-builtin:startswith arg prefix)
                           finally (return i))))
           (let ((next-rest-args (subseq rest-args arg-num)))
             (action-argument argument (subseq rest-args 0 arg-num)
                              parse-result)
             next-rest-args))))
      ((string= nargs "+")
       (with-slots (prefix) parser
         (let ((arg-num (loop
                           for arg in rest-args
                           for i from 0
                           until (clap-builtin:startswith arg prefix)
                           finally (return i))))
           (if (= arg-num 0)
               (error 'too-few-arguments))
           (let ((next-rest-args (subseq rest-args arg-num)))
             (action-argument argument (subseq rest-args 0 arg-num)
                              parse-result)
             next-rest-args))))
       )))

(defmethod process-argument ((parser argument-parser)
                             (argument help-argument)
                             parse-result
                             target-arg rest-args)
  ;; just print the help and exit from it
  (print-help parser)
  (clap-sys:exit 0))

(defun replace-prog (str prog)
  "replace '%(prog)s in `str' to `prog''"
  (clap-builtin:replace str "%(prog)s" prog))

(defgeneric print-usage (parser)
  (:documentation
   "print the usage of `parser'. if it is not specified, the usage will be
generated automatically"))

(defmethod print-usage ((parser argument-parser))
  (with-slots (usage prog) parser
    (if (not (eq usage :generated))
        (progn
          (write-string (replace-prog usage prog))
          (terpri))
        (let ((generated-usage (generate-usage parser)))
          (write-string (replace-prog generated-usage prog))
          (terpri)))))

(defgeneric generate-usage (parser)
  (:documentation
   "generate the string of the usage from argument-parser object."))

(defmethod generate-usage ((parser argument-parser))
  (with-slots (prog) parser
    (format nil "usage: ~A ~A ~A" prog
            (generate-optional-arguments-usage parser)
            (generate-positional-arguments-usage parser))))

(defgeneric optional-arguments (parser)
  (:documentation
   "return a list of the `argument' instances for optional arguments"))

(defmethod optional-arguments ((parser argument-parser))
  (remove-if-not
   #'(lambda (x) (optional-argument-p parser x)) (arguments parser)))

(defgeneric positional-arguments (parser)
  (:documentation
   "return a list of the `argument' instances for positional arguments"))

(defmethod positional-arguments ((parser argument-parser))
  (remove-if
   #'(lambda (x) (optional-argument-p parser x)) (arguments parser)))

(defun argument-format (metavar nargs)
  "return the string to show the parameters of the argument
according to `nargs' and `metavar'."
  (cond
    ((numberp nargs)
     (if (= nargs 0)
         ""
         (clap-builtin:join " "
                            (make-list nargs :initial-element metavar))))
    ((string= nargs "*")
     (format nil "[~A [~A ...]]" metavar metavar)) ; [U [U ...]]
    ((string= nargs "+")
     (format nil "~A [~A ...]" metavar metavar)) ; U [U ...]
    ((string= nargs "?")
     (format nil "[~A]" metavar)))) ; [U]

(defgeneric generate-one-optional-argument-usage (argument parser)
  (:documentation
   "generate the string showing usage of an optional argument."))

(defmethod generate-one-optional-argument-usage ((argument argument)
                                                 (parser argument-parser))
  (let ((short-options
         (remove-if #'(lambda (f) (long-option-p parser f))
                    (flags argument))))
    (let ((option-str
           (if short-options
               (car short-options)
               (car (flags argument)))))
      (with-slots (nargs metavar) argument
        (let ((argument-str (argument-format metavar nargs)))
          (format nil "[~A]"
                  (clap-builtin:join
                   " "

                   (if (string= argument-str "")
                       (list option-str)
                       (list option-str argument-str)))))))))

(defgeneric generate-optional-arguments-usage (parser)
  (:documentation
   "return a string of the usage of optional arguments"))

(defmethod generate-optional-arguments-usage ((parser argument-parser))
  (clap-builtin:join
   " "
   (mapcar #'(lambda (argument)
               (generate-one-optional-argument-usage argument parser))
           (optional-arguments parser))))

(defgeneric generate-one-positional-argument-usage (argument parser)
  (:documentation
   "return a string of the usage of positional arguments"))

(defmethod generate-one-positional-argument-usage ((argument argument)
                                                   (parser argument-parser))
  (with-slots (metavar nargs) argument
    (cond
      ((numberp nargs)
       (clap-builtin:join " " (make-list nargs :initial-element metavar)))
      ((string= nargs "?")
       (error "not implemented yet"))
      ((string= nargs "*")
       (format nil "[~A ...]" metavar))
      ((string= nargs "+")
       (format nil "~A [~A ...]" metavar metavar)))))

(defgeneric generate-positional-arguments-usage (parser)
  (:documentation
   "return a string of the usage of positional arguments"))

(defmethod generate-positional-arguments-usage ((parser argument-parser))
  (clap-builtin:join
   " "
   (mapcar #'(lambda (argument)
               (generate-one-positional-argument-usage argument parser))
           (positional-arguments parser))))

(defgeneric print-description (parser)
  (:documentation
   "print the description of `parser'."))

(defmethod print-description ((parser argument-parser))
  (with-slots (description prog) parser
    (when (and description (not (= (length description) 0)))
      (write-string (replace-prog description prog))
      (terpri)
      (terpri))))

(defgeneric print-positional-argument-help (arg parser offset)
  (:documentation
   "print the help of a positional argument."))

(defmethod print-positional-argument-help ((arg argument)
                                           (parser argument-parser)
                                           offset)
  (with-slots (metavar help) arg
    (let ((help-str (concatenate 'string
                                 "  " metavar
                                 (coerce (make-list
                                          (- offset (length metavar))
                                          :initial-element #\ )
                                         'string)
                                 help)))
      (write-string help-str)
      (terpri))))

(defgeneric print-optional-argument-help (arg parser offset)
  (:documentation
   "print the help of a optional argument."))

(defmethod print-optional-argument-help ((arg argument)
                                         (parser argument-parser)
                                         offset)
  (with-slots (metavar help nargs) arg
    (let ((option-str (clap-builtin:join ", " (flags arg)))
          (argument-str (argument-format metavar nargs)))
      (let ((help-str (concatenate 'string
                                   "  "
                                   option-str
                                   " "
                                   argument-str
                                   (coerce
                                    (make-list (max (- offset
                                                       (+ (length option-str)
                                                          (length argument-str)
                                                          1))
                                                    2)
                                               :initial-element #\ )
                                    'string)
                                   help)))
        (write-string help-str) (terpri)))))

(defgeneric print-positional-arguments (parser offset)
  (:documentation
   "print the help of the positional arguments."))

(defmethod print-positional-arguments ((parser argument-parser) offset)
  (let ((positional-arguments (positional-arguments parser)))
    (when positional-arguments
      (format t "positional arguments:~%")
      (dolist (arg positional-arguments)
        (print-positional-argument-help arg parser offset)
        (terpri)))))


(defgeneric print-optional-arguments (parser offset)
  (:documentation
   "print the help of the optional arguments."))

(defmethod print-optional-arguments ((parser argument-parser) offset)
  (let ((optional-arguments (optional-arguments parser)))
    (format t "optional arguments:~%")
    (dolist (arg optional-arguments)
      (print-optional-argument-help arg parser offset))))

(defgeneric print-epilog (parser)
  (:documentation
   "print the `epilog' of the parser if it is specified."))

(defmethod print-epilog ((parser argument-parser))
  (with-slots (epilog prog) parser
    (if epilog
        (progn
          (write-string (replace-prog epilog prog))
          (terpri)))))

(defgeneric estimate-help-offset (parser)
  (:documentation
   "return the width of offset of help strings"))

(defmethod estimate-help-offset ((parser argument-parser))
  (let ((max-width
         (apply #'max
                (mapcar #'(lambda (x)
                            (if (optional-argument-p parser x)
                                (length (clap-builtin:join
                                         ", " (flags x)))
                                (length (metavar x))))
                        (arguments parser)))))
    (+ 2 max-width)))

(defgeneric print-help (parser)
  (:documentation
   "this is an implementation of ArgumentParser.print_help.
it just prints out the help to stdio."))

(defmethod print-help ((parser argument-parser))
  (let ((offset (estimate-help-offset parser)))
    (print-usage parser)
    (print-description parser)
    (print-positional-arguments parser offset)
    (print-optional-arguments parser offset)
    (print-epilog parser)
    (finish-output)
    t))

(defgeneric make-class-from-options (parse-result)
  (:documentation "return a class, that is an instance of standard-class.
ths slots and thir values are defined by the `arguments' of parser."))

(defmethod make-class-from-options ((parse-result hash-table))
  (let ((dests (clap-builtin:keys parse-result)))
    (let ((anon-class
           (make-instance 'standard-class
                          :direct-slots
                          (mapcar #'(lambda (x) (list :name x))
                                  dests))))
      (make-instance anon-class))))

(defgeneric parsed-options (parse-result)
  (:documentation "inner function of parse-args. this function will return
multiple values of a anonymous class instance to represent options and the list
of remained arguments."))

(defmethod parsed-options ((parse-result hash-table))
  (let ((class (make-class-from-options parse-result)))
    (dolist (arg (clap-builtin:items parse-result))
      (setf (slot-value class (car arg)) (cdr arg)))
    class))

(defgeneric parse-optional-args-rec (parser args parse-result
                                            &key namespace nonmatched-args)
  (:documentation "this is a helper method of parse-args. this function will
parser optional arguments of `args' and store the result into `arguments'
of `parser' and `nonmatched-args'. finally, it will return multiple values
of options and the remaining arguments."))

(defmethod parse-optional-args-rec ((parser argument-parser) args parse-result
                                    &key (namespace nil)
                                    (nonmatched-args nil))
  (if (null args)
      (values parse-result (reverse nonmatched-args))
      (let ((target-arg (car args))
            (rest-args (cdr args)))
        ;; first of all, parse optional argument
        (let ((match-argument (find-match-optional-argument
                               parser target-arg parse-result)))
          (if match-argument
              (parse-optional-args-rec parser
                              (process-argument parser match-argument
                                                parse-result
                                                target-arg rest-args)
                              parse-result
                              :namespace namespace
                              :nonmatched-args nonmatched-args)
              (parse-optional-args-rec
               parser rest-args parse-result
               :namespace namespace
               :nonmatched-args (cons target-arg nonmatched-args)))))))

(defgeneric verificate-arguments (parser result)
  (:documentation
   "verificate the arguments parsed"))

(defmethod verificate-arguments ((parser argument-parser) (result hash-table))
  (with-slots (arguments) parser
    (dolist (arg arguments)
      (with-slots (nargs) arg
        (cond
          ((numberp nargs)
           nil)
          ((string= nargs "+")
           (if (null (clap-builtin:lookup result (dest arg)))
               (error 'too-few-arguments)))
          ;; TODO: required flag
          (t
           nil))))))

(defun minimum-args-num (positional-arguments)
  "return sum of the minimum number of arguments"
  (apply #'+
         (mapcar #'(lambda (arg)
                     (with-slots (nargs) arg
                       (minimum-arg-num nargs)))
                 positional-arguments)))

(defun minimum-arg-num (nargs)
  "return the minumu number of arguments for `nargs'"
  (cond
    ((numberp nargs)
     nargs)
    ((string= nargs "?")
     0)
    ((string= nargs "*")
     0)
    ((string= nargs "+")
     1)))

(defgeneric split-positional-arguments (parser args)
  (:documentation "return the splitted list of `args' according to
the arguments of `parser'."))

(defmethod split-positional-arguments ((parser argument-parser) args)
  (let ((positional-arguments (positional-arguments parser)))
    (let ((min-args-num (minimum-args-num positional-arguments)))
      (cond ((< (length args) min-args-num)
             (error 'too-few-arguments))
            ((= (length args) min-args-num)
             (let ((minarg-list (mapcar #'minimum-arg-num
                                        (mapcar #'nargs positional-arguments))))
               (let ((start-index-list (loop for sum = 0 then (+ sum n)
                                          for n in minarg-list
                                          collect sum)))
                 (loop for prev = 0 then i
                    for i in (append (cdr start-index-list)
                                     (list (length args)))
                    collect (subseq args prev i)))))
            ((> (length args) min-args-num)
             (vararg-split-positional-arguments parser args))))))

(defgeneric vararg-policy (parser args)
  (:documentation
   "return the policy of vararg. the policy are represented by :*, :+, :?
or NIL."))

(defmethod vararg-policy ((parser argument-parser) args)
  (let ((positional-arguments (positional-arguments parser)))
    (let ((narg-list (mapcar #'nargs positional-arguments))
          (args-num (length args)))
      (let ((leftest-* (position "*" narg-list :test #'equal))
            (leftest-+ (position "+" narg-list :test #'equal))
            (leftest-? (position "?" narg-list :test #'equal)))
        (cond
          ((and leftest-* leftest-+)
           (if (< leftest-* leftest-+) :* :+))
          (leftest-* :*)
          (leftest-+ :+)
          (leftest-?                       ;nargs only has ?s
           (let ((?num (length
                        (remove-if-not #'(lambda (x) (and (stringp x)
                                                          (string= x "?")))
                                       narg-list)))
                 (min-args-num (minimum-args-num positional-arguments)))
             (if (<= min-args-num (length args) (+ min-args-num ?num))
                 :?
                 nil)))
          (t nil))))))

;; left-prior rule
;; ? ? * ?
;;       ^ ignored
;; ? ? * *
;;       ^ ignored
;; ? ? * +
;;       ^ only 1
;; ? ? * 1
;;       ^ not-ignored

(defun nargs-list-fill-righter (narg-list leftest)
  "set the number of the arguments which are located in right of `leftest'"
  (loop for nargs in narg-list
     for i from 0
     if (<= i leftest)
     collect nargs
     else if (> i leftest)
     collect (cond
               ((numberp nargs)
                nargs)
               ((string= nargs "?")
                0)
               ((string= nargs "*")
                0)
               ((string= nargs "+")
                1))))

(defun nargs-list-fill-lefter-* (nargs-list rest-num)
  "fill the list of nargs which has * in the most left position."
  (loop for narg in nargs-list
     with rest = rest-num
     if (numberp narg)
     collect narg
     else
     collect
       (if (not (= rest 0))
           (cond
             ((string= narg "?")
              (decf rest)
              1)
             ((string= narg "*")
              (prog1
                  rest
                (setf rest 0)))
             (t
              (error "it might be a bag")))
           (cond
             ((string= narg "?") 0)
             ((string= narg "*") 0)
             (t
              (error "it might be a bag"))))))

(defun nargs-list-fill-lefter-+ (nargs-list rest-num)
  "fill the list of nargs which has + in the most left position."
  (loop for narg in nargs-list
     with rest = rest-num
     if (numberp narg)
     collect narg
     else
     collect
       (cond
         ((= rest 1)
          (cond ((string= narg "?") 0)
                ((string= narg "+") (setf rest 0) 1)
                (t (error "it might be a bag"))))
         ((= rest 0)
          (cond ((string= narg "?") 0)
                (t (error "it might be a bag"))))
         (t
          (cond
            ((string= narg "?")
             (decf rest)
             1)
            ((string= narg "+")
             (prog1
                 rest
               (setf rest 0)))
            (t
             (error "it might be a bag")))))))

(defun nargs-list-fill-? (nargs-list rest-num)
  "fill the list of nargs which has ?s in itself."
  (loop for narg in nargs-list
     with rest = rest-num
     if (numberp narg)
     collect narg
     else
     collect
       (if (= rest 0)
           0
           (progn
             (decf rest)
             1))))

(defgeneric vararg-split-positional-arguments (parser args)
  (:documentation
   "split `args' according to the definitions of the arguments of `parser'.
`args' is required to be parsed as vararg."))

(defmethod vararg-split-positional-arguments ((parser argument-parser) args)
  (let ((indices (vararg-split-positional-arguments-indices parser args)))
    (let ((start-index-list (loop for sum = 0 then (+ sum n)
                               for n in indices
                               collect sum)))
      (loop for prev = 0 then i
         for i in (append (cdr start-index-list) (list (length args)))
         collect (subseq args prev i)))))

(defgeneric vararg-split-positional-arguments-indices (parser args)
  (:documentation
   "return the list which consists of the number of the parameters
of the arguments."))

(defmethod vararg-split-positional-arguments-indices
    ((parser argument-parser) args)
  (let ((vararg-policy (vararg-policy parser args))
        (narg-list (mapcar #'nargs (positional-arguments parser))))
    (case vararg-policy
      (:*
       (let ((leftest-* (position "*" narg-list :test #'equal)))
         (let ((right-filled-nargs-list
                (nargs-list-fill-righter narg-list leftest-*)))
           (let ((rest-num (- (length args)
                              (apply #'+ (remove-if-not
                                          #'numberp right-filled-nargs-list)))))
             (nargs-list-fill-lefter-* right-filled-nargs-list rest-num)))))
      (:+
       (let ((leftest-+ (position "+" narg-list :test #'equal)))
         (let ((right-filled-nargs-list
                (nargs-list-fill-righter narg-list leftest-+)))
           (let ((rest-num (- (length args)
                              (apply #'+ (remove-if-not
                                          #'numberp right-filled-nargs-list)))))
             (nargs-list-fill-lefter-+ right-filled-nargs-list rest-num)))))
      (:?                               ;only includes ?
       (let ((rest-num (- (length args)
                          (length (remove-if-not #'numberp narg-list)))))
         (nargs-list-fill-? narg-list rest-num)))
      (t
       (error "it might be a bug")))))  ;??

(defgeneric parse-positional-args (parser args parse-result &key namespace)
  (:documentation
   "parse the positional arguments and the result as multiple values of
`parse-result' and the remaining args."))

(defmethod parse-positional-args ((parser argument-parser)
                                  args (parse-result hash-table)
                                  &key (namespace nil))
  (let ((splitted-args (split-positional-arguments parser args))
        (positional-arguments (positional-arguments parser)))
    (loop
       for arg in positional-arguments
       for target in splitted-args
       if target
       do (action-argument arg target parse-result))
    (values parse-result nil)))                           ;rest args?

(defgeneric parse-args (parser &optional args &key namespace)
  (:documentation "this is an implementation of ArgumentParser.parse_args."))

(defmethod parse-args ((parser argument-parser)
                       &optional (args (cdr (clap-sys:argv)))
                       &key (namespace nil))
  (let ((parse-result (make-parse-result-dict parser)))
    (handler-case
        (multiple-value-bind
              (parse-result args)
            (parse-optional-args-rec parser args parse-result
                                     :namespace namespace
                                     :nonmatched-args nil)
          (multiple-value-bind
                (parse-result args)
              (parse-positional-args parser args parse-result
                                     :namespace namespace)
            (verificate-arguments parser parse-result)
            (values (parsed-options parse-result) args)))
      (argparse-error
          (c)
        (print-usage parser)
        (format t "~A: error: ~A~%" (prog parser) c)
        (clap-sys:exit 2)))))

(defgeneric make-parse-result-dict (parser)
  (:documentation "return a dictionary which is initialized by default values
of arguments"))

(defmethod make-parse-result-dict ((parser argument-parser))
  (clap-builtin:dict (remove-if
                      #'null
                      (mapcar #'(lambda (a)
                                  (if (dest a)
                                      (cons (dest a) (default a))
                                      nil))
                              (arguments parser)))))
