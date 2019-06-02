(asdf:defsystem #:cl-refal
    :description "REFAL embedded in Common Lisp."
    :author "Drea <drea8an@yandex.ru>"
    :license "GNU Affero v.3 with written permission from author"
    :serial t
    :depends-on ()
    :components ((:file "package")
(:file "src/interpet.lisp")))
