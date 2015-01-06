(defsystem parenml
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :homepage "https://github.com/CommonDoc/parenml"
  :version "0.1"
  :depends-on (:common-doc
               :esrap)
  :components ((:module "src"
                :serial t
                :components
                ((:file "parser")
                 (:file "transform")
                 (:file "emitter"))))
  :description "S-expression markup language."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op parenml-test))))
