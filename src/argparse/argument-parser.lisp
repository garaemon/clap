(in-package :clap-argparse)

(clap-builtin:defconstant* +argument-actions+
  '(:store :store-const
    :store-true :store-false :store-t :store-nil
    :append :append-const
    :version))

(clap-builtin:defconstant* +max-help-offset+ 10)

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
   (parents :accessor parents
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
   (aliases :accessor aliases
            :initarg :aliases
            :initform nil
            :documentation "aliases to prog name. they will be used
when a parser is used as subparser.")
   (usage :accessor usage
          :initarg :usage
          :initform :generated
          :documentation "the string describing the program usage. it will be
generated in default.")
   (help :accessor help
         :initarg :help
         :initform ""
         :documentation "the help of the parser. this will be used when
argument-parser is used as subparser.")
   (subcommands-manager :accessor subcommands-manager
                        :initarg :subcommands-manager
                        :initform nil
                        :documentation "an instance of subcommands-manager.")
   (super-parser :accessor super-parser
                 :initarg :super-parser
                 :initform nil
                 :documentation "super perser of this parser.")
   (default-func :accessor default-func
     :initarg :default-func
     :initform nil
     :documentation "a place holder of the function specified by set-defaults
method.")
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
   (help :initarg :help :initform ""
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

we don't support buffer size specification because it is not supported
by CL open function."))

(defclass subparsers-manager ()
  ((dest :initarg :dest
         :initform nil
         :accessor dest
         :documentation "the name subparser will be stored in `dest' slot of
namespace object")
   (super-parser :initarg :super-parser
                 :initform nil
                 :accessor super-parser
                 :documentation "the super perser of subcommands.")
   (parsers :initarg :parsers
            :initform nil
            :accessor parsers
            :documentation "a list of argument-parsers")
   (title :initarg :title
          :initform nil
          :accessor title
          :documentation "title of subparsers")
   (description :initarg :description
                :initform nil
                :accessor description
                :documentation "description about subcommands")
   (help :initarg :help
         :initform nil
         :accessor help
         :documentation "help of subcommands"))
  (:documentation
   "its a manager of subparsers. it will be created by calling
add_subparsers method of argument-parser."))

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
  (cond
    ((null (dest arg))
     (if (name arg)
         (setf (dest arg) (read-from-string (name arg)))
         (let ((long-option (find-if #'(lambda (x)
                                         (long-option-p parser x)) (flags arg))))
           (if long-option
               (setf (dest arg)
                     (read-from-string (clap-builtin:lstrip
                                        long-option (prefix-chars parser))))
               (setf (dest arg)
                     (read-from-string (clap-builtin:lstrip
                                        (car (flags arg))
                                        (prefix-chars parser))))))))
    ((stringp (dest arg))
     (setf (dest arg) (read-from-string (dest arg)))))
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
                  (remove-if-not #'(lambda (f) (long-option-p parser f)) flags)))
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
      ((or (eq action :store-const)
           (eq action :store-true)
           (eq action :store-t)
           (eq action :store-false)
           (eq action :store-nil)
           (eq action :append-const))
       (setf nargs 0))))
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

(defgeneric add-subparsers (parser &key title description help dest)
  (:documentation
   "implementation of ArgumentParser.add_subparsers

this method allocates a subparsers-manager instance to register
a couple of subcommands."))

(defmethod add-subparsers ((parser argument-parser)
                           &key (title :positional) (description nil) (help nil)
                           (dest nil))
  (let ((subparsers (make-instance 'subparsers-manager
                                   :dest dest
                                   :title title :help help
                                   :description description
                                   :super-parser parser)))
    (setf (subcommands-manager parser) subparsers)
    subparsers))

(defgeneric add-parser (subparsers name &key help aliases)
  (:documentation
   "register a new argument-parser as a subcommand"))

(defmethod add-parser ((subparsers subparsers-manager) name
                       &key (help nil)
                       (aliases nil))
  (let ((parser (make-instance 'argument-parser :prog name :help help
                               :aliases aliases
                               :super-parser (super-parser subparsers))))
    (setf (parsers subparsers) (append (parsers subparsers) (list parser)))
    parser))

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
                         (required nil) (help "") (metavar nil) (dest nil))
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

(defun looks-like-number-p (arg)
  "return t if `arg' can be read as a number"
  (handler-case
      (clap-builtin:int arg)
    (error (c) nil)))

(defgeneric has-negative-number-option-p (parser)
  (:documentation
   "return T if parser has an option which can be read as a
negative number"))

(defmethod has-negative-number-option-p ((parser argument-parser))
  (with-slots (prefix-chars arguments) parser
    (and (string= prefix-chars "-")
         (clap-builtin:any
          (reduce #'append
                  (mapcar #'(lambda (flags)
                              (mapcar
                               #'(lambda (flag)
                                   (if (null flag) nil
                                       (looks-like-number-p flag)))
                               flags))
                          (mapcar #'flags arguments)))))))

(defgeneric find-possible-optional-arguments (parser arg)
  (:documentation "returns a list of the instances of `arguments'
which can be the optional arguments by abbreviating `arg'"))

(defmethod find-possible-optional-arguments ((parser argument-parser) arg)
  (loop for argument in (optional-arguments parser)
     for possible-flags =
       (loop for flag in (flags argument)
          if (clap-builtin:startswith flag arg)
          collect flag)
     if possible-flags
     collect (cons argument possible-flags)))

(defgeneric find-match-optional-argument (parser arg parse-result)
  (:documentation "return an instance of `argument' which is matched
to parse `arg'."))

(defmethod find-match-optional-argument
    ((parser argument-parser) arg parse-result)
  (dolist (argument (optional-arguments parser))
    (dolist (flag (flags argument))
      (cond ((and (long-option-p parser flag)
                  (not (= (clap-builtin:find arg "=") -1)))
             ;; option like --foo=1
             (multiple-value-bind (before partitioner after)
                 (clap-builtin:partition arg "=")
               (if (string= before flag)
                   (return-from find-match-optional-argument
                     (list argument :long-equal flag)))))
            ;; option like --foo 1, -a 1 
            ;; if the option requires some arguments, they are separated
            ;; with a couple of whitespace
            ((string= flag arg)
             (return-from find-match-optional-argument
               (list argument :simple flag))))))
  ;; argument abbreviation
  (let ((abbreviation-arguments (find-possible-optional-arguments parser arg)))
    (cond ((> (length abbreviation-arguments) 1)
           (error 'ambiguous-option :arg arg
                  :options
                  (reduce #'append (mapcar #'cdr abbreviation-arguments))))
          ((= (length abbreviation-arguments) 1)
           (return-from find-match-optional-argument
             (list (car (car abbreviation-arguments))
                   :simple (cadr abbreviation-arguments))))))
  (dolist (argument (optional-arguments parser))
    (dolist (flag (flags argument))
      ;; option like -a1 or -ab
      (if (clap-builtin:startswith arg flag)
          (return-from find-match-optional-argument
            (list argument :continuous flag)))))
  (with-slots (prefix-chars) parser
    (if (clap-builtin:startswith arg prefix-chars)
        (cond ((has-negative-number-option-p parser)
               (error 'no-such-option :option arg))
              ((not (looks-like-number-p arg)) ;-a rather than -1
               (error 'no-such-option :option arg))
              (t )))) ;; pass 
  nil)

(defgeneric convert-type (argument val argname)
  (:documentation
   "convert `val' to the type specified by the argument.
if the type is nil, no conversion is accompleshed."))

(defmethod convert-type ((argument argument) val argname)
  (with-slots (type) argument
    (cond
      ((null type) val)
      ((or (eq type :integer) (eq type :int)
           (eq type 'integer) (eq type 'int))
       (handler-case (clap-builtin:int val) ;check error
         (error (c)
           (error 'invalid-type-error :argument argname
                  :value val :type "int")))
       ;;(clap-builtin:int val))
       )
      ((or (eq type :float)
           (eq type 'float))
       (handler-case (coerce (read-from-string val) 'float) ;check error
         (error (c)
           (error 'invalid-type-error :argument argname
                  :value val :type "float")))
       ;;(coerce (read-from-string val) 'float))
       )
      ((or (eq type :string)
           (eq type :str)
           (eq type 'string)
           (eq type 'str))
       (format nil "~A" val))           ;to string
      ((or (eq type :open)
           (eq type 'open))
       (open val))
      ((typep type 'file-type)
       ;; TODO: conversion from python like mode spec: "r", "w", ...
       (open val :direction (mode type)))
      ((functionp type)                 ;support lambda
       (funcall type val))
      )))

(defgeneric action-argument (argument args parse-result matched-argname)
  (:documentation "this method will process `args' according to the
`action' of `argument'. the supported `action' are :store, :store-const,
:store-true, :store-false, :append, :append-const, :version and lambda form."))

(defmethod action-argument ((argument argument) args parse-result
                            matched-argname)
  (with-slots (choices action nargs const version) argument
    (symbol-macrolet ((value (clap-builtin:lookup parse-result
                                                  (dest argument))))
      (case action
        (:store
         (cond
           ((numberp nargs)
            (if (= nargs 1)
                (setf value (convert-type argument (car args) matched-argname))
                (setf value (mapcar
                             #'(lambda (x)
                                 (convert-type argument x matched-argname))
                             args))))
           ((string= nargs "?")
            (setf value (convert-type argument (car args) matched-argname)))
           ((string= nargs "*")
            (setf value (mapcar #'(lambda (x)
                                    (convert-type argument x matched-argname))
                                args)))
           ((string= nargs "+")
            (setf value (mapcar #'(lambda (x)
                                    (convert-type argument x matched-argname))
                                args))))
         ;; verificate :choices
         (if (and choices (not (member value choices :test #'equal)))
             (error 'invalid-choice :argument (or (name argument)
                                                  (car (flags argument)))
                    :value value :choices choices)))
        (:store-const
         (setf value const))
        ((:store-true :store-t)
         (setf value t))
        ((:store-false :store-nil)
         (setf value nil))
        (:append
         (if (null value)
             (if (= nargs 1)
                 (setf value (convert-type argument args matched-argname))
                 (setf value
                       (list (convert-type argument args matched-argname))))
             (if (= nargs 1)
                 (setf value (append args (convert-type
                                           argument value matched-argname)))
                 (setf value (append value
                                     (list (convert-type
                                            argument args matched-argname))))))
         (if (and choices (not (member value choices :test #'equal)))
             (error 'invalid-choice)))
        (:append-const
         (setf value (append value (list const))))
        (:version
         (write-string version)
         (terpri)
         (clap-sys:exit 0))))))

(defun count-vararg-num (rest-args prefix)
  "return the number of argument until find the string which
starts with `prefix'"
  (loop
     for arg in rest-args
     for i from 0
     until (clap-builtin:startswith arg prefix)
     finally (return i)))

(defun string-to-list (string)
  "convert string into a list of string like:

\"aa\" -> '(\"a\" \"a\")"
  (mapcar #'(lambda (c) (format nil "~c" c)) (coerce string 'string)))

(defgeneric split-continuous-optional-arguments
    (parser argument target-arg)
  (:documentation
   "split a continuous optional argument such as -ab according to
nargs of argument. it returns multiple values of (splitted-list rest-arg)"))

(defmethod split-continuous-optional-arguments
    ((parser argument-parser) (argument argument) target-arg)
  (with-slots (prefix-chars) parser
    (with-slots (nargs) argument
      ;; -abc -> -a
      (let ((optional-argument (subseq target-arg 0 (1+ (length prefix-chars)))))
        (cond ((numberp nargs)          ; nargs must be 0 or 1
               (cond
                 ((= nargs 0)
                  (values (list optional-argument)
                          (concatenate 'string prefix-chars
                                       (subseq target-arg
                                               (1+ (length prefix-chars))))))
                 ((= nargs 1)
                  (values (list optional-argument
                                (subseq target-arg (1+ (length prefix-chars))))
                          nil))
                 (t
                  (error 'n-expected-arguments :num nargs
                         :argument optional-argument))))
              ((or (string= nargs "*") (string= nargs "?") (string= nargs "+"))
               (values (list optional-argument
                                (subseq target-arg (1+ (length prefix-chars))))
                       nil)))))))

(defgeneric process-argument (parser argument parse-result target-arg rest-args
                                     matched-argname
                                     &optional spec)
  (:documentation "this method will call `action-argument' and return
the arguments which should be processed afterwards."))

(defmethod process-argument ((parser argument-parser) (argument argument)
                             parse-result
                             target-arg rest-args
                             matched-argname
                             &optional (spec :simple))
  ;; spec is one of :simple, :long-equal or :continuous
  (cond ((eq spec :long-equal)
         (multiple-value-bind (before partitioner after)
             (clap-builtin:partition target-arg "=")
           (process-argument parser argument parse-result
                             before (cons after rest-args) matched-argname)))
        ((eq spec :continuous)
         (multiple-value-bind
               (splitted-args rest)
             (split-continuous-optional-arguments
              parser argument target-arg)
           (process-argument parser argument parse-result
                             (car splitted-args)
                             (if rest
                                 (append (cdr splitted-args)
                                         (list rest) rest-args)
                                 (append (cdr splitted-args) rest-args))
                             matched-argname)))
        ((eq spec :simple)
         (with-slots (flags nargs) argument
           (cond
             ((numberp nargs)
              (if (<= nargs (length rest-args))
                  (let ((next-rest-args (subseq rest-args nargs)))
                    (action-argument argument
                                     (subseq rest-args 0 nargs)
                                     parse-result matched-argname)
                    next-rest-args)
                  (error 'n-expected-arguments
                         :num nargs :argument matched-argname)))
             ((string= nargs "?")
              (let ((next-rest-args (cdr rest-args)))
                (action-argument argument (car rest-args)
                                 parse-result matched-argname)
                next-rest-args))
             ((string= nargs "*")
              (with-slots (prefix) parser
                (let ((arg-num (count-vararg-num rest-args prefix)))
                  (let ((next-rest-args (subseq rest-args arg-num)))
                    (action-argument argument (subseq rest-args 0 arg-num)
                                     parse-result matched-argname)
                    next-rest-args))))
             ((string= nargs "+")
              (with-slots (prefix) parser
                (let ((arg-num (count-vararg-num rest-args prefix)))
                  (if (= arg-num 0) (error 'too-few-arguments))
                  (let ((next-rest-args (subseq rest-args arg-num)))
                    (action-argument argument (subseq rest-args 0 arg-num)
                                     parse-result matched-argname)
                    next-rest-args)))))))))

(defmethod process-argument ((parser argument-parser)
                             (argument help-argument)
                             parse-result
                             target-arg rest-args
                             matched-argname
                             &optional spec)
  (declare (ignore spec))
  ;; just print the help and exit from it
  (print-help parser)
  (clap-sys:exit 0))

(defun replace-prog (str prog)
  "replace %(prog)s in `str' to `prog'."
  (clap-builtin:replace str "%(prog)s" prog))

(defun replace-default (str default)
  "replace %(default)s in `str' to `default'."
  (clap-builtin:replace str "%(default)s" (format nil "~A" default)))

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

(defgeneric generate-parent-usage (parser)
  (:documentation
   "generate the string of the parents of `parser'"))

(defmethod generate-parent-usage ((parser argument-parser))
  (if (null (super-parser parser))
      (prog parser)
      (concatenate 'string (generate-parent-usage (super-parser parser))
                   " " (prog parser))))

(defgeneric generate-usage (parser)
  (:documentation
   "generate the string of the usage from argument-parser object."))

(defmethod generate-usage ((parser argument-parser))
  (with-slots (prog) parser
    (format nil "usage: ~A ~A ~A" (generate-parent-usage parser)
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

(defun argument-format (metavar nargs choices)
  "return the string to show the parameters of the argument
according to `nargs' and `metavar'."
  (cond
    ((numberp nargs)
     (if (= nargs 0)
         ""
         (clap-builtin:join " "
                            (cond (choices
                                   (list (format nil "{~{~A~^,~}}" choices)))
                                  ((and (listp metavar)
                                        (= (length metavar) nargs))
                                   metavar) ;error check required?
                                  (t
                                   (make-list nargs
                                              :initial-element metavar))))))
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
         (remove-if #'(lambda (f) (long-option-p parser f)) (flags argument))))
    (let ((option-str (if short-options
                          (car short-options)
                          (car (flags argument)))))
      (with-slots (nargs metavar) argument
        (let ((argument-str (argument-format metavar nargs (choices argument))))
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
       (format nil "[~A]" metavar))
      ((string= nargs "*")
       (format nil "[~A ...]" metavar))
      ((string= nargs "+")
       (format nil "~A [~A ...]" metavar metavar)))))

(defgeneric generate-positional-arguments-usage (parser)
  (:documentation
   "return a string of the usage of positional arguments"))

(defmethod generate-positional-arguments-usage ((parser argument-parser))
  (let ((this-parser-usage
         (clap-builtin:join
          " "
          (mapcar #'(lambda (argument)
                      (generate-one-positional-argument-usage argument parser))
                  (positional-arguments parser))))
        (manager (subcommands-manager parser)))
    (let ((subcommand-usage (if manager
                                (clap-builtin:join
                                 "," (mapcar #'prog (parsers manager)))
                                "")))
      (if (string= subcommand-usage "")
          this-parser-usage
          (format nil "{~A} ~A" subcommand-usage this-parser-usage)))))

(defgeneric print-description (parser)
  (:documentation
   "print the description of `parser'."))

(defmethod print-description ((parser argument-parser))
  (with-slots (description prog) parser
    (when (and description (not (= (length description) 0)))
      (write-string (replace-prog description prog))
      (terpri)
      (terpri))))

(defun offset-space-string (offset)
  (coerce (make-list offset :initial-element #\ ) 'string))

(defgeneric print-positional-argument-help (arg parser offset)
  (:documentation
   "print the help of a positional argument."))

(defmethod print-positional-argument-help ((arg argument)
                                           (parser argument-parser)
                                           offset)
  (with-slots (metavar help default) arg
    (with-slots (prog) parser
      (let ((help-str (concatenate 'string
                                   "  " metavar
                                   (coerce (make-list
                                            (- offset (length metavar))
                                            :initial-element #\ )
                                           'string)
                                   (replace-default
                                    (replace-prog help prog)
                                    default))))
        (write-string help-str)
        (terpri)))))

(defgeneric print-optional-argument-help (arg parser offset)
  (:documentation
   "print the help of a optional argument."))

(defmethod print-optional-argument-help ((arg argument)
                                         (parser argument-parser)
                                         offset)
  (with-slots (metavar help nargs default) arg
    (let ((option-str (clap-builtin:join ", " (flags arg)))
          (argument-str (argument-format metavar nargs (choices arg))))
      (with-slots (prog) parser
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
                                     (replace-default (replace-prog help prog)
                                                      default))))
          (write-string help-str) (terpri))))))

(defgeneric print-subcommands-arguments-contents (manager offset)
  (:documentation
   "print the help of subcommands without title"))

(defmethod print-subcommands-arguments-contents ((manager subparsers-manager)
                                                 offset)
  (format t "  {~{~A~^,~}}  ~A~%"
          (mapcar #'prog (parsers manager)) (help manager))
  (dolist (subparser (parsers manager))
    (cond ((help subparser)
           (format t "~A    ~A~%" (prog subparser) (help subparser)))
          ((eq (title manager) :positional)
           (format t "~A~%" (prog subparser))))))

(defgeneric print-subcommands-arguments (manager offset)
  (:documentation "print the help of subcommands with title"))

(defmethod print-subcommands-arguments ((manager subparsers-manager) offset)
  ;; print title
  (format t "~%~A:~%" (title manager))
  (if (description manager)
      (format t "  ~A~%"  (description manager)))
  (print-subcommands-arguments-contents manager offset))

(defgeneric print-positional-arguments (parser offset)
  (:documentation "print the help of the positional arguments."))

(defmethod print-positional-arguments ((parser argument-parser) offset)
  (let ((positional-arguments (positional-arguments parser))
        (manager (subcommands-manager parser)))
    (when (or (and manager
                   (eq (title manager) :positional))
              positional-arguments)
      (format t "positional arguments:~%")
      ;; subcommand
      (when (and manager (eq (title manager) :positional))
        (print-subcommands-arguments-contents manager offset))
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
                                (length (clap-builtin:join ", " (flags x)))
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
    (if (and (subcommands-manager parser)
             (not (eq (title (subcommands-manager parser)) :positional)))
        (print-subcommands-arguments (subcommands-manager parser) offset))
    (print-epilog parser)
    (finish-output)
    t))

(defgeneric parsed-options (parse-result namespace)
  (:documentation "inner function of parse-args. this function will fill 
the namespace object specified by `parse-result'."))

(defmethod parsed-options ((parse-result hash-table) (namespace namespace))
  (dolist (arg (clap-builtin:items parse-result))
    (setf (clap-builtin:lookup namespace (car arg)) (cdr arg)))
  namespace)

(defgeneric parse-optional-args (parser args parse-result &key namespace)
  (:documentation "this is a helper method of parse-args. this function will
parser optional arguments of `args' and store the result into `parse-result'.
finally, it will return a list of options and the remaining arguments."))

(defmethod parse-optional-args ((parser argument-parser) args parse-result
                                &key (namespace nil))
  (with-slots (prefix-chars) parser
    (let* ((-- (concatenate 'string prefix-chars prefix-chars))
           (args* (loop for arg in args until (string= arg --) collect arg))
           (rest-args (if (not (= (length args*) (length args)))
                          (subseq args (1+ (length args*)))
                          nil))
           (has-negative-number-option (has-negative-number-option-p parser))
           (option-poss
            (loop for arg in args* for n from 0
               if (and (clap-builtin:startswith arg prefix-chars)
                       (not (looks-like-number-p arg)))
               collect n
               else if (and (clap-builtin:startswith arg prefix-chars)
                            has-negative-number-option
                            (looks-like-number-p arg))
               collect n)))
      (let ((option-before-args (if option-poss
                                    (subseq args* 0 (car option-poss))))
            (nonmatched-args
             (if option-poss
                 (loop
                    for start-option-pos = (car option-poss) then end-option-pos
                    for end-option-pos in (append (cdr option-poss)
                                                  (list (length args*)))
                    for target = (elt args* start-option-pos)
                    for candidate-args =
                      (if (< (1+ start-option-pos) end-option-pos)
                           (subseq args* (1+ start-option-pos) end-option-pos)
                          nil)
                    for (match-argument spec matched-argname)
                      = (find-match-optional-argument parser target parse-result)
                    if match-argument   ;need?
                    collect
                      (process-argument parser
                                        match-argument parse-result
                                        target candidate-args
                                        matched-argname spec))
                 (list args*))))
        (values parse-result (append option-before-args
                                     (reduce #'append
                                             (remove-if #'null nonmatched-args))
                                     rest-args))))))

(defgeneric verificate-arguments (parser result)
  (:documentation
   "verificate the arguments parsed"))

(defmethod verificate-arguments ((parser argument-parser) (result hash-table))
  (with-slots (arguments) parser
    (dolist (arg arguments)
      (with-slots (flags nargs required) arg
        (cond                           ;verificate nargs
          ((numberp nargs) nil)
          ((string= nargs "+")
           (if (null (clap-builtin:lookup result (dest arg)))
               (error 'too-few-arguments)))
          (t nil))
        (if (and flags required
                 (null (clap-builtin:lookup result (dest arg))))
            (error 'required-option :option (car flags)))))))

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
              (error "it might be a bag [0]")))
           (cond
             ((string= narg "?") 0)
             ((string= narg "*") 0)
             (t
              (error "it might be a bag [1]"))))))

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
                (t (error "it might be a bag [2]"))))
         ((= rest 0)
          (cond ((string= narg "?") 0)
                (t (error "it might be a bag [3]"))))
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
             (error "it might be a bag [4]")))))))

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
      (t           ; has ? option but too many arguments are specified
       (error 'extra-arguments
              :args (subseq
                     args
                     (reduce #'+
                             (mapcar
                              #'(lambda (n)
                                  (cond ((numberp n) n)
                                        ((string= n "?") 1)
                                        (t
                                         (error "it mighe be a bug [5]"))))
                              narg-list))))))))

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
       do (action-argument arg target parse-result (name arg)))
    (values parse-result nil)))                           ;rest args?

(defgeneric match-subcommand (parser arg)
  (:documentation "return a subparser if arg matches one of subcommand
of parser"))

(defmethod match-subcommand ((parser argument-parser) arg)
  (if (subcommands-manager parser)
      (let ((subparsers (parsers (subcommands-manager parser))))
        (find-if
         #'(lambda (parser) (or (string= (prog parser) arg)
                                (clap-builtin:any
                                 (mapcar #'(lambda (x) (string= x arg))
                                         (aliases parser)))))
         subparsers))
      nil))

(defgeneric set-defaults (parser &key func)
  (:documentation
   "this is an implementation of ArgumentParser.set_defaults.

a function registered by `set-defaults' will be abailable via func slot of
namespace."))

(defmethod set-defaults ((parser argument-parser) &key (func nil))
  (if func (setf (default-func parser) func)))

(defgeneric extract-subcommand-args (parser args)
  (:documentation "split args into the arguments for subcommand and
the arguments for the current argument-parser."))

(defmethod extract-subcommand-args ((parser argument-parser) args)
  (let ((manager (subcommands-manager parser)))
    (if manager
        (let ((subparsers (parsers manager)))
          (multiple-value-bind (subparser submatch-pos)
              (loop for arg in args
                 for n from 0
                 for match = (match-subcommand parser arg)
                 if match
                 return (values match n))
            (if subparser
                (values (subseq args (1+ submatch-pos))
                        subparser (subseq args 0 submatch-pos))
                (values nil nil args))))
        (values nil nil args))))

(defgeneric parse-args (parser &optional args namespace)
  (:documentation "this is an implementation of ArgumentParser.parse_args."))

(defmethod parse-args ((parser argument-parser)
                       &optional (args (cdr (clap-sys:argv)))
                       (namespace (make-instance 'namespace)))
  (let ((parse-result (make-parse-result-dict parser)))
    (handler-case
        (multiple-value-bind
              (subcommand-args subparser args)
            (extract-subcommand-args parser args)
          (if (and subparser (subcommands-manager parser)
                   (dest (subcommands-manager parser)))
              (setf (clap-builtin:lookup namespace
                                         (dest (subcommands-manager parser)))
                    (prog subparser)))
          (multiple-value-bind
                (parse-result args)
              (parse-optional-args parser args parse-result
                                   :namespace namespace)
            (multiple-value-bind
                  (parse-result args)
                (parse-positional-args parser args parse-result
                                       :namespace namespace)
              (verificate-arguments parser parse-result)
              (if args (error 'unrecognized-arguments :args args))
              (if (and subparser (default-func subparser))
                  (setf (func namespace) (default-func subparser)))
              (if subparser
                  (concat-namespace (parsed-options parse-result namespace)
                                    (parse-args subparser subcommand-args))
                  (parsed-options parse-result namespace)))))
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
