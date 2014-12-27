(in-package :cl-user)
(defpackage parenml-test
  (:use :cl :fiveam))
(in-package :parenml-test)

(def-suite tests
  :description "parenml tests.")
(in-suite tests)

(test parser
  (is
   (equal (parenml.parser:parse-string "test")
          "test"))
  (is
   (equal (parenml.parser:parse-string ":test")
          :|test|))
  (is
   (equal (parenml.parser:parse-string "(1 2 3)")
          (list "1 2 3")))
  (is
   (equal (parenml.parser:parse-string "(a (b (c)))")
          (list "a " (list "b " (list "c")))))
  (is
   (equal (parenml.parser:parse-string "(a (b (c) d) e)")
          (list "a " (list "b " (list "c") " d") " e"))))

(run! 'tests)
