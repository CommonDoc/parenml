(in-package :cl-user)
(defpackage parenml.parser
  (:use :cl :esrap)
  (:export :parse-string)
  (:documentation "Parse ParenML input into an S-expression."))
(in-package :parenml.parser)

(defrule whitespace (+ (or #\Space #\Tab #\Newline))
  (:constant nil))

(defrule valid-char (not (or whitespace #\( #\))))

(defrule atom (+ valid-char)
  (:lambda (text)
    (let* ((text (text text))
           (keyword (char= (elt text 0) #\:)))
      (list :type (if keyword
                      :keyword
                      :atom)
            :value (if keyword
                       (subseq text 1)
                       text)))))

(defrule list (and #\( (* sexp) (? whitespace) #\))
 (:destructure (open items ws close)
   (declare (ignore open ws close))
    (list :type :list
          :value items)))

(defrule sexp (and (? whitespace) (or list atom) (? whitespace))
  (:destructure (left-ws content right-ws)
    (declare (ignore left-ws right-ws))
    content))

(defun parse-string (string)
  "Parse a ParenML string into an S-expression."
  (parse 'sexp string))
