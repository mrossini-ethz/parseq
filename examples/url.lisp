(require :asdf)
(asdf:load-system :parseq)
(use-package :parseq)

;; This example sets up the rules for parsing generic and http URLs
;; and then parses an example URL.

(defrule url () (or generic httpaddress))
(defrule generic () (and scheme ":" path (? (and "?" search))))
(defrule scheme () ialpha (:string))
(defrule httpaddress () (and "http://" hostport (? (and #\/ path)) (? (and #\? search)))
  (:destructure (scheme (host port) path search)
                (list scheme
                      host
                      (if port (second port))
                      (if path (second path))
                      (if search (second search)))))
(defrule hostport () (and host (? (and #\: port))))
(defrule host () (or hostname hostnumber) (:string))
(defrule hostname () (and ialpha (? (and #\. hostname))))
(defrule hostnumber () (and digits #\. digits #\. digits #\. digits))
(defrule port () digits (:string) (:function #'parse-integer))
(defrule path () (and xpalphas (? (and #\/ path))) (:string))
(defrule search () (and xalphas (? (and #\+ search))) (:destructure (str more) (append (list str) (if more (second more)))))
(defrule xalpha () (or alpha digit safe extra escape))
(defrule xalphas () (+ xalpha) (:string))
(defrule xpalpha () (or xalpha #\+))
(defrule xpalphas () (+ xpalpha))
(defrule ialpha () (and alpha (? xalphas)))
(defrule alpha () char (:test (x) (alpha-char-p x)))
(defrule digit () char (:test (x) (digit-char-p x)))
(defrule digits () (+ digit))
(defrule safe () (or #\$ #\- #\_ #\@ #\. #\&))
(defrule extra () (or #\! #\* #\" #\' #\( #\) #\; #\, #\Space))
(defrule escape () (and #\% hex hex))
(defrule hex () (or digit #\a #\b #\c #\d #\e #\f #\A #\B #\C #\D #\E #\F))

(trace-rule 'url :recursive t)
(parseq 'url "http://github.com:22/mrossini-ethz/parseq?foo+bar+baz" :parse-error t)
