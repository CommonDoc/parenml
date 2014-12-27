(in-package :cl-user)
(defpackage parenml-test
  (:use :cl :fiveam))
(in-package :parenml-test)

;;; Utils

(defun extract-doc-text (doc)
  (with-output-to-string (stream)
    (common-doc:traverse-document
     doc
     #'(lambda (node)
         (when (typep node 'common-doc:<text-node>)
           (write-string (common-doc:text node) stream))))))

(defun equal-parse (string sexp)
  (equal (parenml.parser:parse-string string) sexp))

(defun equal-transform (class input text)
  (let ((output (parenml.transform:transform input)))
    (and (typep output class)
         (equal (extract-doc-text output) text))))

(defmacro test-markup-transform (tag class)
  `(equal-transform ',class (list ,tag "test") "test"))

;;; Tests

(def-suite tests
  :description "ParenML tests.")
(in-suite tests)

(test parser
  (is-true
   (equal-parse "test" "test"))
  (is-true
   (equal-parse ":test" :|test|))
  (is-true
   (equal-parse "(1 2 3)" (list "1 2 3")))
  (is-true
   (equal-parse "(a (b (c)))"
                (list "a " (list "b " (list "c")))))
  (is-true
   (equal-parse "(a (b (c) d) e)"
                (list "a " (list "b " (list "c") " d") " e"))))

(test transform
  (is-true
   (equal (common-doc:text (parenml.transform:transform "test"))
          "test"))
  (signals simple-error
    (parenml.transform:transform :test))
  (is-true
   (test-markup-transform :|b| common-doc:<bold>))
  (is-true
   (test-markup-transform :|i| common-doc:<italic>))
  (is-true
   (test-markup-transform :|u| common-doc:<underline>))
  (is-true
   (test-markup-transform :|strike| common-doc:<strikethrough>))
  (is-true
   (test-markup-transform :|code| common-doc:<code>))
  (is-true
   (test-markup-transform :|sup| common-doc:<superscript>))
  (is-true
   (test-markup-transform :|sub| common-doc:<subscript>))
  (is-true
   (test-markup-transform :|q| common-doc:<inline-quote>))
  (is-true
   (test-markup-transform :|quote| common-doc:<block-quote>)))

(run! 'tests)
