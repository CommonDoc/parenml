(in-package :cl-user)
(defpackage parenml-test.parser
  (:use :cl :fiveam)
  (:export :tests))
(in-package :parenml-test.parser)

(def-suite tests
  :description "ParenML parser  tests.")
(in-suite tests)

(defmacro with-parse ((parsed-type parsed-value input) &rest body)
  (let ((parsed-name (gensym)))
    `(let* ((,parsed-name (parenml.parser:parse-string ,input))
            (,parsed-type (getf ,parsed-name :type))
            (,parsed-value (getf ,parsed-name :value)))
       ,@body)))

(test parser
  (with-parse (type value "test")
    (is
     (equal type :atom))
    (is
     (equal value "test")))
  (with-parse (type value ":key")
    (is
     (equal type :keyword))
    (is
     (equal value "key"))))
