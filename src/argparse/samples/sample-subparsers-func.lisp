(require :clap-argparse)

(import '(clap-builtin:split))

(defun foo (args)
  (format t "~A~%" (* (clap-builtin:. args x) (clap-builtin:. args y))))

(defun bar (args)
  (format t "((~A))~%" (clap-builtin:. args z)))

(let ((parser (make-instance 'clap-argparse:argument-parser)))
  (let ((subparsers (clap-argparse:add-subparsers parser)))
    (let ((parser-foo (clap-argparse:add-parser subparsers "foo")))
      (clap-argparse:add-argument parser-foo "-x" :type :int :default 1)
      (clap-argparse:add-argument parser-foo "y" :type :float)
      (clap-argparse:set-defaults parser-foo :func #'foo))
    (let ((parser-bar (clap-argparse:add-parser subparsers "bar")))
      (clap-argparse:add-argument parser-bar "z")
      (clap-argparse:set-defaults parser-bar :func #'bar))
    (let ((args (clap-argparse:parse-args parser (split "foo 1 -x 2"))))
      (funcall (clap-builtin:. args clap-argparse:func) args))
    (let ((args (clap-argparse:parse-args parser (split "bar XYZYX"))))
      (funcall (clap-builtin:. args clap-argparse:func) args))))
