(in-package :cl-user)
(defpackage parenml.transform
  (:use :cl)
  (:import-from :common-doc
                :doc
                :<text-node>)
  (:export :transform)
  (:documentation "Turn parser output into a CommonDoc document."))
(in-package :parenml.transform)

;;; Methods

(defgeneric transform (sexp)
  (:documentation "Transform an S-expression into a CommonDoc node."))

(defparameter *transforms* (make-hash-table))

(defmethod transform ((list list))
  "Transform a list."
  (let ((first (first list)))
    (assert (and (symbolp first)
                 (eq (symbol-package first) (find-package :keyword))))
    (let ((operator (gethash first *transforms*)))
      (if operator
          (funcall operator (rest list))
          (error "Unknown operator: ~A." operator)))))

(defmethod transform ((string string))
  "Transform a string."
  (doc <text-node> (:text string)))

(defmethod transform ((keyword symbol))
  "Transform a keyword."
  ;; All the symbols returned by the parser are guaranteed to be keywords.
  (error "Keywords can't occur outside the first element in a list."))

;;; Transforms

(defmacro define-transform (name (&rest args) &rest body)
  `(setf (gethash ,name *transforms*)
         #'(lambda (list)
             (destructuring-bind ,args list
               ,@body))))

