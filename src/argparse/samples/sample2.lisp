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

;; ?, *, +
(let ((parser (make-instance 'clap-argparse:argument-parser)))
  (clap-argparse:add-argument parser "A")
  (clap-argparse:add-argument parser "B")
  (clap-argparse:add-argument parser "C")
  (describe (clap-argparse:parse-args parser '("AA" "BB" "CC"))))

(let ((parser (make-instance 'clap-argparse:argument-parser)))
  (clap-argparse:add-argument parser "A")
  (clap-argparse:add-argument parser "B" :nargs "?")
  (clap-argparse:add-argument parser "C")
  (describe (clap-argparse:parse-args parser '("AA" "CC")))
  (describe (clap-argparse:parse-args parser '("AA" "BB" "CC"))))

(let ((parser (make-instance 'clap-argparse:argument-parser)))
  (clap-argparse:add-argument parser "A")
  (clap-argparse:add-argument parser "B" :nargs "*")
  (clap-argparse:add-argument parser "C")
  (describe (clap-argparse:parse-args parser '("AA" "CC")))
  (describe (clap-argparse:parse-args parser '("AA" "BB" "CC")))
  (describe (clap-argparse:parse-args parser '("AA" "BB1" "BB2" "BB3" "CC"))))
