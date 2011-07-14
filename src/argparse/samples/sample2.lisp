(require :clap-argparse)

;; epilog sample
(let ((parser (make-instance 'clap-argparse:argument-parser
                             :description "A foo that bars"
                             :epilog "And that's how you'd foo a bar")))
  (clap-argparse:print-help parser))

;; :add-help sample
(let ((parser (make-instance 'clap-argparse:argument-parser
                             :prog "PROG"
                             :add-help nil)))
  (clap-argparse:add-argument parser "--foo"
                              :help "foo help")
  (clap-argparse:print-help parser))

;; prefix-chars usage
(let ((parser (make-instance 'clap-argparse:argument-parser
                             :prog "PROG"
                             :prefix-chars "+")))
  (clap-argparse:print-help parser))

(let ((parser (make-instance 'clap-argparse:argument-parser
                             :prog "PROG"
                             :prefix-chars "+")))
  (clap-argparse:add-argument parser "+f")
  (clap-argparse:add-argument parser "++bar")
  (describe (clap-argparse:parse-args parser
                                      (clap-builtin:split "+f X ++bar Y"))))

;; parent sample
(let ((parent-parser (make-instance 'clap-argparse:argument-parser
                                    :add-help nil)))
  (clap-argparse:add-argument parent-parser "--parent" :type 'int)
  (let ((foo-parser (make-instance 'clap-argparse:argument-parser
                                   :parents (list parent-parser))))
    (clap-argparse:add-argument foo-parser "foo")
    (describe (clap-argparse:parse-args foo-parser
                                        (clap-builtin:split "--parent 2 XXX"))))
  (let ((bar-parser (make-instance 'clap-argparse:argument-parser
                                   :parents (list parent-parser))))
    (clap-argparse:add-argument bar-parser "--bar")
    (describe (clap-argparse:parse-args
               bar-parser
               (clap-builtin:split "--parent 2 --bar YYY")))))

;; type
(let ((parser (make-instance 'clap-argparse:argument-parser)))
  (clap-argparse:add-argument parser "foo" :type :int)
  (clap-argparse:add-argument parser "bar" :type :open)
  (describe
   (clap-argparse:parse-args parser
                             (clap-builtin:split "2 sample2.lisp"))))

(let ((parser (make-instance 'clap-argparse:argument-parser)))
  (clap-argparse:add-argument parser "bar" :type (make-instance
                                                  'clap-argparse:file-type
                                                  :mode :output))
  (describe
   (clap-argparse:parse-args parser
                             (clap-builtin:split "hoge.txt"))))

(let ((parser (make-instance 'clap-argparse:argument-parser :prog "PROG")))
  (clap-argparse:add-argument
   parser "foo" :type #'(lambda (string)
                          (let* ((value (clap-builtin:int string))
                                 (sqrt (sqrt value)))
                            (if (not (= sqrt (floor sqrt)))
                                (error 'clap-argparse:argument-type-error
                                       :format-control "~A is not a perfect square"
                                       :format-arguments `(,string)))
                            value)))
  (describe
   (clap-argparse:parse-args parser
                             (clap-builtin:split "9")))
  (describe
   (clap-argparse:parse-args parser
                             (clap-builtin:split "7"))))


