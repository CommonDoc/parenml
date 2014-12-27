(in-package :cl-user)
(defpackage parenml-test
  (:use :cl :fiveam))
(in-package :parenml-test)

(def-suite tests
  :description "ParenML tests.")
(in-suite tests)

(defun equal-parse (string sexp)
  (equal (parenml.parser:parse-string string) sexp))

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

(run! 'tests)
