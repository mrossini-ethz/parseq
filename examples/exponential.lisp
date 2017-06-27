(require :asdf)
(asdf:load-system :parseq)
(use-package :parseq)

;; Some parsing expression grammars lead to exponential processing time.
;; Here is an example of such a case:

(with-local-rules
  (defrule a () "a")
  (defrule b () (or (and a "b") a))
  (defrule c () (or (and b "c") b))
  (defrule d () (or (and c "d") c))
  (defrule e () (or (and d "e") d))
  (defrule f () (or (and e "f") e))
  (defrule g () (or (and f "g") f))
  (defrule h () (or (and g "h") g))
  (defrule i () (or (and h "i") h))
  (defrule j () (or (and i "j") i))
  (trace-rule 'j :recursive t)
  (format t "Without packrat parsing:~%")
  (parseq 'j "a"))

;; With packrat parsing enabled:

(with-local-rules
  (defrule a () "a" (:packrat t))
  (defrule b () (or (and a "b") a) (:packrat t))
  (defrule c () (or (and b "c") b) (:packrat t))
  (defrule d () (or (and c "d") c) (:packrat t))
  (defrule e () (or (and d "e") d) (:packrat t))
  (defrule f () (or (and e "f") e) (:packrat t))
  (defrule g () (or (and f "g") f) (:packrat t))
  (defrule h () (or (and g "h") g) (:packrat t))
  (defrule i () (or (and h "i") h) (:packrat t))
  (defrule j () (or (and i "j") i) (:packrat t))
  (trace-rule 'j :recursive t)
  (format t "With packrat parsing:~%")
  (parseq 'j "a"))

;; From the tracing output it is clear, that packrat parsing is much faster.
;; Packrat parsing guarantees linear time at the cost of increased memory usage.
;; However, changing the parsing grammar expression yields an even better performance,
;; even without packrat parsing. It is unclear whether all such cases can be
;; simplified in this way.

(with-local-rules
  (defrule a () "a")
  (defrule b () (? "b"))
  (defrule c () (? "c"))
  (defrule d () (? "d"))
  (defrule e () (? "e"))
  (defrule f () (? "f"))
  (defrule g () (? "g"))
  (defrule h () (? "h"))
  (defrule i () (? "i"))
  (defrule j () (? "j"))
  (defrule abcdefghij () (and a b c d e f g h i j))
  (trace-rule 'abcdefghij :recursive t)
  (format t "Different parsing grammar expression, without packrat:~%")
  (parseq 'abcdefghij "a"))

;; Another example: The original rules, but packrat parsing is enabled only for every other rule.

(with-local-rules
  (defrule a () "a" (:packrat t))
  (defrule b () (or (and a "b") a) (:packrat nil))
  (defrule c () (or (and b "c") b) (:packrat t))
  (defrule d () (or (and c "d") c) (:packrat nil))
  (defrule e () (or (and d "e") d) (:packrat t))
  (defrule f () (or (and e "f") e) (:packrat nil))
  (defrule g () (or (and f "g") f) (:packrat t))
  (defrule h () (or (and g "h") g) (:packrat nil))
  (defrule i () (or (and h "i") h) (:packrat t))
  (defrule j () (or (and i "j") i) (:packrat nil))
  (trace-rule 'j :recursive t)
  (format t "Original parsing grammar expression, with packrat parsing (every other rule):~%")
  (parseq 'j "a"))

