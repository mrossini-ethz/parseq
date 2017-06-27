(defsystem "parseq"
  :description "A parser for sequences such as strings, lists, vectors as well as trees."
  :version "0.4.0"
  :author "Marco Rossini"
  :license "GPLv2"
  :serial t
  :components ((:file "package")
               (:file "conditions")
               (:file "utils")
               (:file "defrule"))
  :in-order-to ((test-op (test-op :parseq-test))))

(defsystem "parseq-test"
  :description "Unit testing for parseq."
  :author "Marco Rossini"
  :license "GPLv2"
  :depends-on (:parseq)
  :serial t
  :components ((:file "test/unit-test")
               (:file "test/test")))

(defmethod perform ((operation test-op) (system (eql (find-system :parseq-test))))
  (funcall (intern "PARSEQ-TEST" :parseq)))
