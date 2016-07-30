(in-package :asdf-user)

(defsystem "parser"
  :description "Parser"
  :version "0.1"
  :author "Marco Rossini"
  :components ((:file "package")
               (:file "list-parser" :depends-on ("package" "utils"))
               (:file "utils" :depends-on ("package"))
               ))
