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
  (if (add-help parser)
      (setf (arguments parser)
            (cons (make-help-argument parser) (arguments parser))))
  )
                                       

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
   (version :initarg :version :initform nil
            :accessor version))
  (:documentation
   "this is a class to represent an argument or an optional."))

(defclass help-argument (argument)
  ()
  (:documentation
   "this is a special class to represent -h or --help option."))

(defgeneric make-help-argument (parser)
  (:documentation
   "make a help-argument instance using `argument-parser'."))

(defmethod make-help-argument ((parser argument-parser))
  (make-instance 'help-argument
                 :flags '("-h" "--help")
                 :nargs 0
                 :help "show this help message and exit"))

(defmethod print-object ((object argument) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A [~s]" (action object)
            (or (name object) (flags object)))))

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
    ;; check duplication
    (let ((names-and-flags
           (mapcan #'(lambda (x) (if (name x) (name x) (flags x)))
                   (arguments parser))))
      (dolist (name-or-flag (if (listp name-or-flags)
                                name-or-flags
                                (list name-or-flags)))
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
  (:documentation "create an instance of argument class."))

(defmethod make-argument ((parser argument-parser) name-or-flags &rest args)
  ;; error check
  (let ((prefix (prefix-chars parser)))
    (cond
      ((not (listp name-or-flags))      ; force to be a list
       (apply #'make-argument parser (list name-or-flags) args))
      ((> (length name-or-flags) 1) ; all the flags should start with prefix
       (if (clap-builtin:all (mapcar #'(lambda (x)
                                         (clap-builtin:startswith x prefix))
                                     name-or-flags))
           (apply #'make-instance 'argument :flags name-or-flags args)
           (error "there is a flag which does not start with ~A in ~A"
                  prefix name-or-flags)))
      ((clap-builtin:startswith (car name-or-flags) prefix) ;starts with "-"
       (apply #'make-instance 'argument :flags name-or-flags args))
      (t
       (apply #'make-instance 'argument :name (car name-or-flags) args)))))

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

(defgeneric find-match-argument (parser arg)
  (:documentation "return an instance of `argument' which is matched
to parse `arg'."))

(defmethod find-match-argument ((parser argument-parser) arg)
  (dolist (argument (arguments parser))
    (if (name argument)
        ;; positional arguments
        (if (string= (name argument) arg)
            (return-from find-match-argument argument)))
    ;; optional arguments
    (dolist (flag (flags argument))
      (cond ((and (long-option-p parser flag)
                  (= (clap-builtin:find arg "=") 1))
             (multiple-value-bind (before partitioner after)
                 (clap-builtin:partition arg "=")
               (if (string= before flag)
                   (return-from find-match-argument argument))))
            ((string= flag arg)
             (return-from find-match-argument argument))))))

(defgeneric action-argument (argument args parse-result)
  (:documentation "this method will process `args' according to the
`action' of `argument'. the supported `action' are :store, :store-const,
:store-true, :store-false, :append, :append-const, :version and lambda form."))

(defmethod action-argument ((argument argument) args parse-result)
  (with-slots (action nargs const version) argument
    (symbol-macrolet ((value (clap-builtin:lookup parse-result argument)))
      (case action
        (:store
         (if (= nargs 1)
             (setf value (car args))
             (setf value args)))
        (:store-const                       ;TODO: const is not supported
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
                 (setf value args)
                 (setf value (list args)))
             (if (= nargs 1)
                 (setf value (append value args))
                 (setf value (append value (list args))))))
        (:append-const                      ;TODO: const is not supported
         (error "not implemented yet"))
        (:version
         (format t version)
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

(defgeneric generate-one-optional-argument-usage (argument parser)
  (:documentation
   "generate the string showing usage of an optional argument."))

(defmethod generate-one-optional-argument-usage ((argument argument)
                                                 (parser argument-parser))
  (let ((short-options
         (remove-if #'(lambda (f) (long-option-p parser f))
                    (flags argument))))
    (if short-options
        (format nil "[~A]" (car short-options))
        (format nil "[~A]" (car (flags argument))))))

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
       (error "not implemented yet"))
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
  (with-slots (metavar help) arg
    (let ((option-str (clap-builtin:join ", " (flags arg))))
      (let ((help-str (concatenate 'string
                                   "  "
                                   option-str
                                   (coerce
                                    (make-list (- offset (length option-str))
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
  (let ((dests (mapcar #'car (clap-builtin:items parse-result))))
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

;; implementingx
(defmethod parsed-options ((parse-result hash-table))
  (let ((class (make-class-from-options parse-result)))
    (dolist (arg (clap-builtin:items parse-result))
      (setf (slot-value class (car arg)) (cdr arg)))
    class))

(defgeneric parse-args-rec (parser args parse-result
                            &key namespace nonmatched-args)
  (:documentation "this is a helper method of parse-args. this function will
parser `args' and store the result into `arguments' of `parser'
and `nonmatched-args'. finally, it will return multiple values of options
and the remaining arguments."))

(defmethod parse-args-rec ((parser argument-parser) args parse-result
                           &key (namespace nil) (nonmatched-args nil))
  (if (null args)
      (values (parsed-options parse-result) (reverse nonmatched-args))
      (let ((target-arg (car args))
            (rest-args (cdr args)))
        (let ((match-argument (find-match-argument parser target-arg)))
          (if match-argument
              (parse-args-rec parser
                              (process-argument parser match-argument
                                                parse-result
                                                target-arg rest-args)
                              parse-result
                              :namespace namespace
                              :nonmatched-args nonmatched-args)
              (parse-args-rec
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
           (if (null (clap-builtin:lookup result arg))
               (error 'too-few-arguments)))
          ;; TODO: required flag
          (t
           nil))))))

(defgeneric parse-args (parser &optional args &key namespace)
  (:documentation "this is an implementation of ArgumentParser.parse_args. "))

(defmethod parse-args ((parser argument-parser)
                       &optional (args (cdr (clap-sys:argv)))
                       &key (namespace nil))
  (let ((parse-result (make-parse-result-dict parser)))
    (handler-case
        (prog1
            (parse-args-rec parser args parse-result
                            :namespace namespace :nonmatched-args nil)
          (verificate-arguments parser parse-result))
      (argparse-error
          (c)
        (print-usage parser)
        (format t "~A: error: ~A~%" (prog parser) c)
        (clap-sys:exit 2)))))

(defgeneric make-parse-result-dict (parser)
  (:documentation "return a dictionary which is initialized by default values
of arguments"))

(defmethod make-parse-result-dict ((parser argument-parser))
  (clap-builtin:dict (mapcar #'(lambda (a)
                                 (cons (dest a) (default a)))
                             (arguments parser))))

#|
(progn
  (require :clap-argparse)
  (setq p (make-instance 'clap-argparse:argument-parser))
  (clap-argparse:add-argument p '("-p" "-q"))
  (clap-argparse:add-argument p '("-a"))
  (describe (clap-argparse::parse-args p (clap-builtin:split "-q 1 -a 2"))))

(progn
  (setq p (make-instance 'clap-argparse:argument-parser))
  (clap-argparse:add-argument p '("--foo") :action :append)
  (describe (clap-argparse::parse-args p (clap-builtin:split "--foo 1 --foo 2")))
  )
|#
