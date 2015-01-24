(in-package :cl-user)
(defpackage parenml.parser
  (:use :cl :esrap)
  (:export :parse-string)
  (:documentation "Parse ParenML input into an S-expression."))
(in-package :parenml.parser)

(defrule whitespace (+ (or #\Space #\Tab #\Newline))
  (:constant nil))

(defrule atom (+ (not (or #\( #\))))
  (:lambda (text)
    (text text)))

(defrule keyword (and #\: (+ (not (or #\( #\) whitespace))))
  (:destructure (colon text)
    (declare (ignore colon))
    (intern (text text) :keyword)))

(defrule list (and #\( (* sexp) #\))
  (:destructure (open items close)
    (declare (ignore open close))
    items))

(defrule sexp (and (or list keyword atom))
  (:destructure (content)
    content))

(defun parse-string (string)
  "Parse a ParenML string into an S-expression."
  (parse 'sexp string))
