(require :asdf)
(asdf:load-system :parseq)
(use-package :parseq)

;; This example parses a date in RFC5322 format, e.g. "Thu, 13 Jul 2017 13:28:03 +0200".

(defrule date-time () (and (? (and day-of-week ",")) date time) (:choose '(0 0) 1 2) (:flatten))

(defrule day-of-week () (or "Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

(defrule date () (and day month year))

(defrule day () (and (? FWS) (rep (1 2) digit) FWS) (:choose 1) (:string) (:function #'parse-integer))

(defrule month () (or "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defrule year () (and FWS (rep 4 digit) FWS) (:choose 1) (:string) (:function #'parse-integer))

(defrule time () (and time-of-day FWS zone) (:choose 0 2) (:flatten))

(defrule time-of-day () (and hour ":" minute (? (and ":" second))) (:choose 0 2 '(3 1)))

(defrule hour () (rep 2 digit) (:string) (:function #'parse-integer))

(defrule minute () (rep 2 digit) (:string) (:function #'parse-integer))

(defrule second () (rep 2 digit) (:string) (:function #'parse-integer))

(defrule zone () (and (or "+" "-") (rep 4 digit)) (:string))

(defrule FWS () #\space)

(trace-rule 'date-time :recursive t)

(parseq 'date-time "Thu, 13 Jul 2017 13:28:03 +0200")
