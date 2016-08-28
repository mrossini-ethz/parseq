(in-package :asdf-user)

(defsystem "parseq"
  :description "A parser for all kinds of sequences (and even trees)."
  :version "0.1"
  :author "Marco Rossini"
  :components ((:file "package")
               (:file "list-parser" :depends-on ("package" "utils"))
               (:file "utils" :depends-on ("package"))
               ))
