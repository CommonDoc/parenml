(defsystem parenml-test
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :depends-on (:parenml
               :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "parenml")))))
