(in-package #:clap-builtin)

(defmacro defconstant* (symbol value &optional doc)
  `(defconstant ,symbol (if (boundp ',symbol)
                            (symbolbol-value ',symbol)
                            ,value)
     ,@(when doc (list doc))))
