(in-package :asdf-user)

(defsystem "parseq"
  :description "A parser for sequences such as strings, lists, vectors as well as trees."
  :version "0.1"
  :author "Marco Rossini"
  :license "GPLv2"
  :components ((:file "package")
               (:file "utils" :depends-on ("package"))
               (:file "defrule" :depends-on ("package" "utils")))
  :in-order-to ((test-op (test-op :parseq-test))))

(defsystem "parseq-test"
  :description "Unit testing for parseq."
  :author "Marco Rossini"
  :license "GPLv2"
  :depends-on (:parseq)
  :components ((:file "test/unit-test")
               (:file "test/test" :depends-on ("test/unit-test"))))

(defmethod perform ((operation test-op) (system (eql (find-system :parseq-test))))
  (funcall (intern "PARSEQ-TEST" :parseq)))
