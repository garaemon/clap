(in-package :clap-string)

(defclass template ()
  ((template :accessor template
             :initarg :template
             :documentation "TEMPLATE slot stores the string passed to
TEMPLATE instance. you should not change this slot however read-only
access is not enforced.")
   (delimiter :accessor delimiter
              :initarg :delimiter
              :initform "$"
              :documentation "DELIMITER is the string to lead the ids to
be substituted.
DELIMITER should not be a regular expression.
defaults to \"$\"")
   (idpattern :accessor idpattern
              :initarg :idpattern
              :initform  "[_a-z][_a-z0-9]*"
              :documentation "IDPATTERN is the regular expression to describe
the ids without braces. it defaults to [_a-z][_a-z0-9]*."))
  (:documentation
   "this is an implementation of string.Template.

TEMPLATE provides the simple APIs to substitute strings.
substitution is performed with the following rules:
  - If you want to use delimiter itself in substituted string,
you can escape the delimiter string by placing delimiter."))

(defun make-template (template
                      &key (delimiter "$") (idpattern "[_a-z][_a-z0-9]*"))
  (make-instance 'template :template template
                 :delimiter delimiter
                 :idpattern idpattern))

(defgeneric substitute (template mapping &rest kws)
  (:documentation
   "this is an implementation of string.template.substitute"))

#|
(clap-string:substitute
 (clap-string:make-template
  "$$hoge $hoge $a $$odiejw oiajdeoj fuga piyo ${hoge fuga} $aabbcc")
 (clap-builtin:dict '(("hoge" . "HOGE")
                      ("a" . "A")
                      ("hoge fuga" . "HOGE FUGA")
                      ("aabbcc" . "AABBCC"))
                    :test #'equal))
|#
(defmethod substitute ((template template) (mapping hash-table) &rest kws)
  (let* ((delimiter (delimiter template))
         (quoted-delimiter (cl-ppcre:quote-meta-chars delimiter))
         (template-str (template template)))
    (labels ((all-matches-pos (suffix)
               (cl-ppcre:all-matches
                (concatenate 'string quoted-delimiter suffix) template-str)))
      (let ((escaped-delimiter-pos (all-matches-pos quoted-delimiter))
            (non-braced-id-pos (all-matches-pos (idpattern template)))
            (braced-id-pos (all-matches-pos "{.*}")))
        ;; before calling %substitute, we make sure $$foo like string
        (let ((non-braced-id-pos-safe
               (remove-escaped-delimiter-positions
                escaped-delimiter-pos non-braced-id-pos))
              (braced-id-pos-safe
               (remove-escaped-delimiter-positions
                escaped-delimiter-pos braced-id-pos)))
          (let ((output (make-string-output-stream)))
            (%substitute output template mapping
                         0 escaped-delimiter-pos
                         non-braced-id-pos-safe braced-id-pos-safe)
            (get-output-stream-string output)))))))

(defun remove-escaped-delimiter-positions (escaped braced)
  "helper function for substitute.

REMOVE-ESCAPED-DELIMITER-POSITIONS will remove the positions following
the escaped delimiters.

the template string may have the escaped delimiter and some characters
 following that. for example: $$foo. $$foo should be replaced by $foo
and is not recognized any identifier."
  (if (null escaped)
      braced
      (destructuring-bind (start finish &rest escaped) escaped
        ;; if the template string is "foo $$foo foo",
        ;; BRACED has (1- FINISH) in its even number element.
        (let ((pos (position (1- finish) braced)))
          (if pos
              (let ((new-braced (append (subseq braced 0 pos)
                                        (subseq braced (+ 2 pos)))))
                (remove-escaped-delimiter-positions escaped new-braced))
              (remove-escaped-delimiter-positions escaped braced))))))

(defun %substitute (output template mapping
                    i escaped non-braced braced)
  "helper function for substitute.

%SUBSTITUTE is a recursive function to write substituted string into OUTPUT
stream."
  (let* ((template-str (template template))
         (delimiter (delimiter template))
         (template-str-len (length template-str)))
    (labels ((substitute-mapping (from to)
               (let ((key (subseq template-str (1+ from) to)))
                 (write-string (clap-builtin:lookup mapping key) output))))
      (cond ((= template-str-len i) output)
            ((and escaped (= i (car escaped)))
             (write-string (delimiter template) output)
             (%substitute output template mapping
                          (+ (* 2 (length delimiter)) i)
                          (cddr escaped) non-braced braced))
            ((and non-braced (= i (car non-braced)))
             (substitute-mapping (car non-braced) (cadr non-braced))
             (%substitute output template mapping
                          (+ i (- (cadr non-braced) (car non-braced)))
                          escaped (cddr non-braced) braced))
            ((and braced (= i (car braced)))
             (substitute-mapping (1+ (car braced)) (1- (cadr braced)))
             (%substitute output template mapping
                          (+ i (- (cadr braced) (car braced)))
                          escaped non-braced (cddr braced)))
          (t
           (write-char (elt template-str i) output)
           (%substitute output template mapping
                        (1+ i) escaped non-braced braced))))))
  

(defgeneric safe-substitute (template mapping &rest kws))

