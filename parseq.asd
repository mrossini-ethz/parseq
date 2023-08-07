(defsystem "parseq"
  :description "A library for parsing sequences such as strings and lists using parsing expression grammars."
  :version "0.5.0"
  :author "Marco Rossini"
  :license "GPLv2"
  :serial t
  :components ((:file "package")
               (:file "conditions")
               (:file "utils")
               (:file "defrule"))
  :in-order-to ((test-op (test-op :parseq/test))))

(defsystem "parseq/test"
  :description "Unit testing for parseq."
  :author "Marco Rossini"
  :license "GPLv2"
  :depends-on (:parseq)
  :serial t
  :components ((:file "test/unit-test")
               (:file "test/test")))

(defmethod perform ((operation test-op) (system (eql (find-system :parseq/test))))
  (funcall (intern "PARSEQ-TEST")))
