(in-package :cl-user)
(defpackage parenml.transform
  (:use :cl :common-doc)
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
    (cond
      ((keywordp first)
        (let ((operator (gethash first *transforms*)))
          (if operator
              (funcall operator (rest list))
              (error "Unknown operator: ~A." operator))))
      ((stringp first)
       (transform (cons :|text| list)))
      (t
       (error "Invalid first element of a list: ~A." first)))))

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

(defmacro define-content-transform (keyword class)
  `(define-transform ,keyword (&rest children)
     (make-instance ',class
                    :children (loop for child in children collecting
                                (transform child)))))

;; Text blocks

(define-transform :|text| (&rest content)
 ;; Text blocks are just groupings of text
 (loop for elem in content collecting (transform elem)))

;; Paragraphs

(define-transform :|p| (&rest blocks)
  ;; The input to this is a list of elements, each of which is either a plain
  ;; old fashioned list or a block of some kind
  (loop for block in blocks collecting
    (let ((transformed (transform block)))
      (if (listp transformed)
          (make-instance '<paragraph>
                         :children transformed)
          transformed))))

;; Markup

(define-content-transform :|b| <bold>)
(define-content-transform :|i| <italic>)
(define-content-transform :|u| <underline>)
(define-content-transform :|strike| <strikethrough>)
(define-content-transform :|code| <code>)
(define-content-transform :|sup| <superscript>)
(define-content-transform :|sub| <subscript>)

;; Quotes

(define-content-transform :|q| <inline-quote>)
(define-content-transform :|quote| <block-quote>)

;; Links

;; Lists

(define-content-transform :|item| <list-item>)

(define-transform :|def| (term &rest definition)
  (make-instance '<definition>
                 :term (transform term)
                 :definition (transform definition)))

(define-transform :|list| (&rest items)
  (make-instance '<unordered-list>
                 :items (transform items)))

(define-transform :|olist| (&rest items)
  (make-instance '<ordered-list>
                 :items (transform items)))

(define-transform :|deflist| (&rest items)
  (make-instance '<definition-list>
                 :items (transform items)))

;; Figures

;; Tables

(define-content-transform :|cell| <cell>)

(define-transform :|row| (&rest cells)
  (make-instance '<row>
                 :cells
                 (loop for cell in cells collecting
                   (transform cell))))

(define-transform :|table| (&rest rows)
  (make-instance '<table>
                 :rows
                 (loop for row in rows collecting
                   (transform row))))
