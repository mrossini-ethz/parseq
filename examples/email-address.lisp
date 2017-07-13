(require :asdf)
(asdf:load-system :parseq)
(use-package :parseq)

;; This example  demonstrates the parsing  expression grammar for  a simplified
;; e-mail  address using  parseq. Simplified  e-mail addresses  allow only  the
;; special characters ".", "_" and "-" in the part before the "@" (local part).
;; This is  not a standard, but  the convention of many  e-mail providers. Note
;; that despite the simplification, according  to RFC5322, RFC5321 and RFC3696,
;; the character "."  is not allowed at  the beginning or the end  of the local
;; part and may not be repeated consecutively.

;; Warning: This is an example that demonstrates the use of parseq. It does not
;; guarantee  that   the  grammar  rules  described   therein  are  implemented
;; correctly.

;; Parsing expression grammar -------------------------------------------------

;; Return a list with the local part and the domain.
(defrule email-address () (and local-part "@" domain)
  (:lambda (local at domain)
    (declare (ignore at))
    (list local domain)))

;; The  local  part  consists  of   multiple  characters.  The  characters  are
;; concatenated to a  string by the (:string) directive. Test  for special case
;; of the "." character (see above).
(defrule local-part () (+ (char "a-zA-Z0-9._-"))
  (:string)
  (:not (str)
        (or (char= (elt str 0) #\.)
            (char= (elt str (1- (length str))) #\.)
            (search ".." str)
            (> (length str) 64))))

;; The domain must not be longer than 253 characters (including dots).
(defrule domain () (and label (* (and "." label)))
  (:string)
  (:test (x)
         (<= (length x) 253)))

;; A label may not  start or end with with "-".  The maximum number of
;; characters is 63.
(defrule label () (rep (1 63) (char "a-zA-Z0-9-"))
  (:string)
  (:not (str)
        (or (char= (elt str 0) #\-)
            (char= (elt str (1- (length str))) #\-))))

;; Application ----------------------------------------------------------------

;; Trace the main rule recursively.
(trace-rule 'email-address :recursive t)

;; Apply the parsing expression grammar to an example address.
(parseq 'email-address "foo.bar@example.com")
