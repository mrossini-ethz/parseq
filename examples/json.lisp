(require :asdf)
(asdf:load-system :parseq)
(use-package :parseq)

;; Rules according to RFC8259, December 2017

;; Whitespace (do not return the actual whitespace because it's discarded anyway)
(defrule ws () (* #\Space #\Tab #\Newline #\Return) (:constant t))
;; Delimiters: use a helper rule to simplify the actual rules
(defrule delimiter (c) (and ws c ws) (:choose 1))
(defrule begin-array () (delimiter #\[))
(defrule begin-object () (delimiter #\{))
(defrule end-array () (delimiter #\]))
(defrule end-object () (delimiter #\}))
(defrule name-separator () (delimiter #\:))
(defrule value-separator () (delimiter #\,))
;; Main entry rule
(defrule json-text () (and ws value ws) (:choose 1))
;; Literals (use symbols)
(defrule true () "true" (:constant 'TRUE))
(defrule false () "false" (:constant 'FALSE))
(defrule null () "null" (:constant 'NULL))
;; Value uses many different rules
(defrule value () (or obj array num str true false null))
;; Process objects into lists of members (association list)
(defrule obj () (and begin-object (and member (* value-separator member)) end-object)
  ;; Discard the brackets
  (:choose 1)
  ;; Flatten the list once
  (:lambda (a b) (concatenate 'list (list a) b))
  ;; Discard the separators
  (:lambda (&rest items) (loop for i in items for j upfrom 0 when (zerop (mod j 2)) collect i)))
;; Process members into conses
(defrule member () (and str name-separator value) (:choose 0 2) (:function #'cons))
;; JSON uses C-style strings. Here, escape sequences are ignored except for the \" escape.
(defrule str () (and #\" (* (and #\\ char) (not #\")) #\") (:choose 1) (:string))
;; Convert numbers into lisp numbers
(defrule num () (and (? #\-) int (? frac) (? exp)) (:lambda (sign int frac exp) (* (if sign -1 1) (+ int (if frac frac 0)) (if exp (expt 10 exp) 1))))
;; Integer part of a number -> convert to integer
(defrule int () (or #\0 (and (char "1-9") (* digit))) (:string) (:function #'parse-integer))
;; Fractional part of a number -> convert to fraction
(defrule frac () (and #\. (+ digit)) (:choose 1) (:string) (:lambda (x) (/ (parse-integer x) (expt 10 (length x)))))
;; Exponent part of a number -> convert to integer
(defrule exp () (and (or #\e #\E) (? #\+ #\-) (+ digit)) (:choose 1 2) (:string) (:function #'parse-integer))
;; Process arrays into vectors of values
(defrule array () (and begin-array (and value (* value-separator value)) end-array)
  ;; Discard the brackets
  (:choose 1)
  ;; Flatten the list once
  (:lambda (a b) (concatenate 'list (list a) b))
  ;; Discard the separators
  (:lambda (&rest items) (loop for i in items for j upfrom 0 when (zerop (mod j 2)) collect i))
  ;; Apply the vector function to convert the list into a vector
  (:function #'vector))

;; Turn on rule tracing (to visualize what the parser is doing)
(trace-rule 'json-text :recursive t)

;; Parse some JSON data
(parseq 'json-text "
{
  \"firstName\": \"John\",
  \"lastName\": \"Smith\",
  \"isAlive\": true,
  \"age\": 27,
  \"address\": {
    \"streetAddress\": \"21 2nd Street\",
    \"city\": \"New York\",
    \"state\": \"NY\",
    \"postalCode\": \"10021-3100\"
  },
  \"phoneNumbers\": [
    {
      \"type\": \"home\",
      \"number\": \"212 555-1234\"
    },
    {
      \"type\": \"office\",
      \"number\": \"646 555-4567\"
    }
  ],
  \"children\": [
    \"Catherine\",
    \"Thomas\",
    \"Trevor\"
  ],
  \"spouse\": null
}")

;; Output from the parser:
;;
;; (("firstName" . "John") ("lastName" . "Smith") ("isAlive" . TRUE) ("age" . 27)
;;  ("address" ("streetAddress" . "21 2nd Street") ("city" . "New York")
;;   ("state" . "NY") ("postalCode" . "10021-3100"))
;;  ("phoneNumbers"
;;   . #((("type" . "home") ("number" . "212 555-1234"))
;;       (("type" . "office") ("number" . "646 555-4567"))))
;;  ("children" . #("Catherine" "Thomas" "Trevor")) ("spouse" . NULL))
