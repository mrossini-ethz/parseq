(require :asdf)
(asdf:load-system :parseq)
(use-package :parseq)

;; This example demonstrates the parsing expression grammar for URLs
;; using parseq.

;; Warning: This is an example that demonstrates the use of parseq. It
;; does not guarantee that the grammar rules described therein are
;; implemented correctly.

;; The main parsing expression
(defrule url () (and scheme (? authority) (? path) (? query) (? fragment)) (:flatten))

;; The scheme must begin with an alphabetic character
(defrule scheme () (and (+ (char "a-zA-Z0-9.+-")) ":") (:string) (:test (str) (alpha-char-p (elt str 0))))

(defrule authority () (and "//" (? (and user-info "@")) host (? (and ":" port))) (:choose '(1 0) 2 '(3 1)))

;; The user-info is not limited to user:pass, but can be almost anything
(defrule user-info () (* (or %hex (char "a-zA-Z0-9._~!$&'()*+,;=:-"))) (:string))

;; Escaped values are in hexadecimal, e.g. "%2F"
(defrule %hex () (and #\% (rep 2 (char "0-9a-fA-F"))) (:string))

;; The domain must not be longer than 253 characters (including dots)
(defrule host () (and label (* (and "." label))) (:string) (:test (x) (<= (length x) 253)))

;; A label may not  start or end with with "-".  The maximum number of
;; characters is 63.
(defrule label () (rep (1 63) (char "a-zA-Z0-9-")) (:string) (:not (str) (or (char= (elt str 0) #\-) (char= (elt str (1- (length str))) #\-))))

;; The port is just a series of digits. This can be easily converted
;; to an integer.
(defrule port () (* digit) (:string) (:function #'parse-integer))

;; The path grammar is simplified here.
(defrule path () (* (and "/" segment)) (:string))

;; Path segment
(defrule segment () (* (or %hex (char "a-zA-Z0-9._~!$&'()*+,;=:@-"))))

;; Query can be almost anything
(defrule query () (and #\? (* (char "a-zA-Z0-9._~!$&'()*+,;=:@/?-"))) (:choose 1) (:string))

;; Fragment can be almost anything
(defrule fragment () (and #\# (* (char "a-zA-Z0-9._~!$&'()*+,;=:@/?-"))) (:choose 1) (:string))

(trace-rule 'url :recursive t)

(parseq 'url "https://user:pass@github.com:22/mrossini-ethz/parseq?foo+bar+baz#top" :parse-error t)

