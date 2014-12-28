(in-package :cl-user)
(defpackage parenml.emitter
  (:use :cl :common-doc)
  (:export :transform)
  (:documentation "Create an S-expression from a CommonDoc document."))
(in-package :parenml.emitter)

(defgeneric emit (node)
  (:documentation "Create a ParenML representation of a CommonDoc document."))

(defmethod emit ((list list))
  "Emit a list."
  (loop for elem in list do (emit elem)))

(defmacro define-emitter (class &rest body)
  "Define an emitter method."
  `(defmethod emit ((node ,class))
     ,@body))

(defmacro define-child-emitter (class &rest body)
  "Define a simple emitter for elements with `children`."
  `(define-emitter ,class
     (let ((children (emit (children node))))
       ,@body)))

(define-emitter <text-node>
    (text node))
