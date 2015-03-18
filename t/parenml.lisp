(in-package :cl-user)
(defpackage parenml-test
  (:use :cl :fiveam))
(in-package :parenml-test)

(run! 'parenml-test.parser:tests)
