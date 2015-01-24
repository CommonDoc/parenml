(in-package :cl-user)
(defpackage parenml.emitter
  (:use :cl :common-doc)
  (:export :transform)
  (:documentation "Create an S-expression from a CommonDoc document."))
(in-package :parenml.emitter)
