(in-package :clap-builtin)

;; utility functions to define class methods
(defun extract-elements-until-symbols (parse-list symbols)
  "extract the elements of PARSE-LIST until one of SYMBOLS
is found"
  (if (or (null parse-list) (member (car parse-list) symbols))
      (cl:values nil parse-list)
      (multiple-value-bind
            (next-result rest)
          (extract-elements-until-symbols
           (cdr parse-list) symbols)
        (cl:values (cons (car parse-list) next-result)
                   rest))))

(defun split-specialized-lambda-list (args)
  "split a specialized lambda-list into multiple values"
  (multiple-value-bind (var-list args)
      (extract-elements-until-symbols
       args '(&optional &rest &key &allow-other-keys &aux))
    (multiple-value-bind (optional-list args)
        (extract-elements-until-symbols
         (if (eq (car args) '&optional) (cdr args) args)
         '(&rest &key &allow-other-keys &aux))
      (multiple-value-bind (rest args)
          (extract-elements-until-symbols
           (if (eq (car args) '&rest) (cdr args) args)
           '(&key &allow-other-keys &aux))
        (multiple-value-bind (key-list args)
            (extract-elements-until-symbols
             (if (eq (car args) '&key) (cdr args) args)
             '(&allow-other-keys &aux))
          (let ((not-allow-other-keys (if (eq (car args) '&allow-other-keys)
                                          (cdr args) args)))
            (let ((aux-list (if (eq (car not-allow-other-keys) '&aux)
                                (cdr not-allow-other-keys)
                                not-allow-other-keys)))
              (cl:values var-list optional-list
                         rest key-list aux-list))))))))

(defun applicable-specialized-lambda-list (args)
  "convert a specialized lambda list to APPLY-able list.

 example::

   (applicable-specialized-lambda-list '(a b c)) => (A B C nil)
   (applicable-specialized-lambda-list '((a class1) (b class2))) => (A B nil)
   (applicable-specialized-lambda-list '(a &key b (c nil))) => (A :B B :C C nil)
   (applicable-specialized-lambda-list '(a &optional (b nil)) => (A B nil)
   (applicable-specialized-lambda-list '(a &rest args)) => (A ARGS)"
  ;; var optional rest key allow-other-keys aux
  (multiple-value-bind (var-list optional-list rest key-list aux-list)
      (split-specialized-lambda-list args)
    (let ((var-list* (mapcar #'(lambda (x) (if (listp x) (car x) x))
                             var-list))
          (optional-list* (mapcar #'(lambda (x) (if (listp x) (car x) x))
                                  optional-list))
          (rest* (if (listp rest) (car rest) rest))
          (key-list* (mapcan
                      #'(lambda (x)
                          (if (listp x)
                              (if (keywordp (car x))
                                  (list (car x) (cadr x)) ; (:key key)
                                  (list (intern (string (car x))
                                                :keyword)
                                        (car x))) ; (key key-value)
                              (list (intern (string x) :keyword)
                                    x)))
                      key-list)))
      (if rest*
          (append var-list* optional-list* key-list*
                  (list rest*))
          (append var-list* optional-list* key-list* (list nil))))))


(defun enumerate-all-classes (&optional (result nil)
                              (prepended (list (find-class 't))))
  (if (null prepended)
      result
      (let ((subclasses (closer-mop:class-direct-subclasses (car prepended))))
        (let ((new-subclasses (remove-if
                               #'(lambda (c)
                                   (member c (append prepended result)))
                               subclasses)))
          (enumerate-all-classes (cons (car prepended) result)
                                 (append new-subclasses (cdr prepended)))))))


(defvar *built-in-class-instances-table*
  `((type-error . ,(make-condition 'type-error
                                   :expected-type 'number
                                   :datum "a"))
    (program-error . ,(make-condition 'program-error))
    (parse-error . ,(make-condition 'parse-error))
    (control-error . ,(make-condition 'control-error))
    (end-of-file . ,(make-condition 'end-of-file))
    (reader-error . ,(make-condition 'reader-error))
    (stream-error . ,(make-condition 'stream-error))
    (file-error . ,(make-condition 'file-error))
    (package-error . ,(make-condition 'package-error))
    (unbound-variable . ,(make-condition 'unbound-variable))
    (undefined-function . ,(make-condition 'undefined-function))
    (unbound-slot . ,(make-condition 'unbound-slot))
    (cell-error . ,(make-condition 'cell-error))
    (division-by-zero . ,(make-condition 'division-by-zero))
    (floating-point-overflow . ,(make-condition 'floating-point-overflow))
    (floating-point-underflow . ,(make-condition 'floating-point-underflow))
    (floating-point-inexact . ,(make-condition 'floating-point-inexact))
    (floating-point-invalid-operation
     . ,(make-condition 'floating-point-invalid-operation))
    (arithmetic-error . ,(make-condition 'arithmetic-error))
    (print-not-readable . ,(make-condition 'print-not-readable))
    (error . ,(make-condition 'error))
    (storage-condition . ,(make-condition 'storage-condition))
    (serious-condition . ,(make-condition 'serious-condition))
    (style-warning . ,(make-condition 'style-warning))
    (warning . ,(make-condition 'warning))
    (simple-warning . ,(make-condition 'simple-warning))
    (simple-error . ,(make-condition 'simple-error))
    (simple-type-error . ,(make-condition 'simple-type-error
                                          :expected-type 'number
                                          :datum "a"))
    (simple-condition . ,(make-condition 'simple-condition))
    (condition . ,(make-condition 'condition))
    (package . ,(allocate-instance (find-class 'package)))
    (broadcast-stream . ,(allocate-instance (find-class 'broadcast-stream)))
    (synonym-stream . ,(allocate-instance (find-class 'synonym-stream)))
    (echo-stream . ,(allocate-instance (find-class 'echo-stream)))
    (two-way-stream . ,(allocate-instance (find-class 'two-way-stream)))
    (concatenated-stream . ,(allocate-instance
                             (find-class 'concatenated-stream)))
    (restart . ,(allocate-instance (find-class 'restart)))
    (random-state . ,(make-random-state))
    (hash-table . ,(allocate-instance (find-class 'hash-table)))
    (readtable . ,(allocate-instance (find-class 'readtable)))
    (logical-pathname . ,(allocate-instance (find-class 'logical-pathname)))
    (pathname . ,(allocate-instance (find-class 'pathname)))
    (structure-object . ,(allocate-instance (find-class 'structure-object)))
    (method . ,(allocate-instance (find-class 'method)))
    (method-combination . ,(allocate-instance
                            (find-class 'method-combination)))
    (standard-method . ,(allocate-instance (find-class 'standard-method)))
    (standard-generic-function
     . ,(allocate-instance (find-class 'standard-generic-function)))
    (generic-function . ,(allocate-instance (find-class 'generic-function)))
    (standard-class . ,(allocate-instance (find-class 'standard-class)))
    (structure-class . ,(allocate-instance (find-class 'structure-class)))
    (built-in-class . ,(allocate-instance (find-class 'built-in-class)))
    (class . ,(allocate-instance (find-class 'class)))
    (standard-object . ,(allocate-instance (find-class 'standard-object)))
    (function . #'(lambda ()))
    (file-stream . :sealed-class)
    (string-stream . :sealed-class)
    (stream . :sealed-class)
    (character . #\a)
    (symbol . a)
    (bignum . ,(1+ most-positive-fixnum))
    (fixnum . 0)
    (integer . :sealed-class)
    (ratio . 1/2)
    (rational . :sealed-class)
    (double-float . 0.0d0)
    (single-float . 0.0f0)
    (float . :sealed-class)
    (real . :sealed-class)
    (complex . #c(0 0))
    (number . :sealed-class)
    (simple-array . :sealed-class)
    (array . :sealed-class)
    (null . nil)
    (cons . (1))
    (list . :sealed-class)
    (base-string . :sealed-class)
    (simple-base-string . :sealed-class)
    (simple-string . "")          ; actually simple-character-string?
    (string . :sealed-class)
    (simple-bit-vector . #*1)
    (bit-vector . :sealed-class)
    (simple-vector . #())
    (vector . :sealed-class)
    (sequence . :sealed-class)
    (t . :sealed-class))
  "associated list of built-in-classes and its objects")

(defun lookup-built-in-class-object (class)
  "retnrn an object of built-in-class. if no object can be created for
the class, it returns :sealed-class."
  (cdr (assoc class *built-in-class-instances-table*)))

(defmacro define-class-method-wrapper (name args &optional (documentation nil))
  ;; args => ((class class-name) arg2 arg3 ...)
  (let ((class-obj (gensym))
        (built-in-object (gensym)))
    `(defmethod ,name ((,(car (car args)) symbol) ,@(cdr args))
       ,(if documentation documentation
            "this method was automatically generated by
DEFINE-CLASS-METHOD-WRAPPER.")
       (let ((,class-obj (find-class ',(cadr (car args)))))
         (if (typep ,class-obj 'closer-mop:built-in-class)
             (let ((,built-in-object (lookup-built-in-class-object
                                      ,(caar args))))
               (case ,built-in-object
                 (:sealed-class
                  (error 'not-implemented-yet))
                 (t
                  (apply (function ,name) ,built-in-object
                         ,@(applicable-specialized-lambda-list (cdr args))))))
             (apply (function ,name) (allocate-instance ,class-obj)
                    ,@(applicable-specialized-lambda-list (cdr args))))))))

(defmacro define-class-method (name args &rest body)
  ;; args => ((class class-name-symbol) arg2 arg3 ...)
  (let ((class-name-symbol (cadr (car args))))
    (let ((class-instance (lookup-built-in-class-object class-name-symbol)))
      (case class-instance
        (:sealed-class
         `(progn
            (defmethod ,name ((,(caar args) (eql ',(cadr (car args))))
                              ,@(cdr args))
              ,@body)
            (defmethod ,name ,args
              ,@body)))
        (t
         `(defmethod ,name ,args ,@body)
         )))))

#|
(require :clap-builtin)
 (clap-builtin:define-class-method-wrapper integer-method ((obj integer)))
(clap-builtin:define-class-method integer-method ((obj integer))
   'this-is-an-integer-method)
(clap-builtin:define-class-method integer-method ((obj real))
   'this-is-real-method)
(integer-method 1) ; -> this-is-an-integer
(integer-method 'fixnum) ; -> (integer-method 1) -> this-is-an-integer-method
(integer-method 'integer) ; -> this-is-an-integer-method
|#
