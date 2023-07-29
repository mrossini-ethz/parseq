(require :asdf)
(asdf:load-system :parseq)
(use-package :parseq)

;; This  file shows  multiple examples  involving the  parsing of  mathematical
;; expressions: The first example demonstrates the basics of rules and parses a
;; simple  mathematical expression  without  changing anything  in the  parsing
;; result. The second  example performs some basic  transformations to simplify
;; the  result. The  third example  performs more  advanced transformations  to
;; clean up the  parsing result and make it human  readable. The fourth example
;; (actually  3b) transforms  the result  into a  valid lisp  s-expression. You
;; could  call  that  a  compiler.  In  the  fifth  example,  the  mathematical
;; expression  is  evaluated during  parsing  and  the  parsing result  is  the
;; calculated result.
;; All examples parse the mathematical term ((12 - 3) * 4) ^ (1 / (5 + -3)) and
;; for  each of  them the  result  is different,  because different  processing
;; options are used. The  grammar is always exactly the same  way and the rules
;; are repeated for each example.



;; Example 1 - No transformations ---------------------------------------------

;; The 'space' rule matches zero or multiple space characters.
(defrule space () (* #\Space))
;; A value is an optional sign and a series of digits. Therefore, only integers
;; are supported here.
(defrule value () (and (? #\-) (+ (char "0-9"))))
;; The 'bracket'  rule allows recursion  of the  rules. It calls  the toplelvel
;; rule  'sum'.  This  is  sensible because  expressions  within  brackets  are
;; completely self-contained.  Because of  the bracket  character, there  is no
;; left recursion.
(defrule bracket () (and #\( sum #\)))
;; Here, a term is either a value or a bracket expression.
(defrule term () (or value bracket))
;; We  use  the 'binary'  rule  to  simplify  subsequent  rules by  using  rule
;; parameters.  This is  because each  binary  operator will  allow some  space
;; characters left and right and it would be tedious to repeat the same parsing
;; rule over and over.
(defrule binary (symb) (and space symb space))
;; The binary operators are defined using the 'binary' rule using the character
;; that they should match as a parameter.
(defrule add () (binary #\+))
(defrule sub () (binary #\-))
(defrule mul () (binary #\*))
(defrule div () (binary #\/))
(defrule exp () (binary #\^))
;; The 'power' rule processes a  term (value or bracket expression), optionally
;; with an  exponent which  can again  be a  power. This  is the  operator with
;; highest precedence in this grammar.
(defrule power () (and term (? (and exp power))))
;; The 'product' rule  processes one or more a power  expressions, separated by
;; the multiply or division operator.
(defrule product () (and power (* (and (or mul div) power))))
;; The 'sum' rule  processes one or more product expressions,  separated by the
;; addition  and  subtraction  operators.  It   is  the  operator  with  lowest
;; precedence and it represents the top rule for this grammar.
(defrule sum () (and product (* (and (or add sub) product))))

;; Turn on rule tracing to visualize the parsing process
(trace-rule 'sum :recursive t)

;; The  toplevel rule  is  'sum'. The  following expression  uses  the rule  to
;; successfully parse a moderately complex mathematical expression. The result,
;; however, is a horrible mess of nested lists containing single characters and
;; NIL.
(parseq 'sum "((12 - 3) * 4) ^ (1 / (5 + -3))")

;; Result of the previous expression:
;;
;; ((((#\(
;;     ((((#\(
;;         ((((NIL (#\1 #\2)) NIL) NIL)
;;          ((((#\ ) #\- (#\ )) (((NIL (#\3)) NIL) NIL))))
;;         #\))
;;        NIL)
;;       ((((#\ ) #\* (#\ )) ((NIL (#\4)) NIL))))
;;      NIL)
;;     #\))
;;    (((#\ ) #\^ (#\ ))
;;     ((#\(
;;       ((((NIL (#\1)) NIL)
;;         ((((#\ ) #\/ (#\ ))
;;           ((#\(
;;             ((((NIL (#\5)) NIL) NIL)
;;              ((((#\ ) #\+ (#\ )) (((#\- (#\3)) NIL) NIL))))
;;             #\))
;;            NIL))))
;;        NIL)
;;       #\))
;;      NIL)))
;;   NIL)
;;  NIL)
;;
;; There is  excessive nesting because of  the many rules that  are applied and
;; each of them generates a level of  nesting. There are also many NILs because
;; of optional parts in the parsing rules.  But it shows you what the parser is
;; actually doing when processing the input. It  would be nice to clean this up
;; to make  it more  usable. This could  be done by  using some  elaborate post
;; processing code after parsing is finished  or - much better - perform simple
;; processing during parsing. See the examples below.



;; Example 2 - Simple transformations -----------------------------------------

(defrule space () (* #\Space))
;; Concatenate the characters of a number into a string.
(defrule value () (and (? #\-) (+ (char "0-9"))) (:string))
;; Discard the brackets in a bracket expression.
(defrule bracket () (and #\( sum #\)) (:choose 1))
(defrule term () (or value bracket))
;; Discard the spaces around binary operators.
(defrule binary (symb) (and space symb space) (:choose 1))
;; Use a lisp symbol instead of text characters for better readability.
(defrule add () (binary #\+) (:constant '+))
(defrule sub () (binary #\-) (:constant '-))
(defrule mul () (binary #\*) (:constant '*))
(defrule div () (binary #\/) (:constant '/))
(defrule exp () (binary #\^) (:constant '^))
;; Let's leave these as they are for now.
(defrule power () (and term (? (and exp power))))
(defrule product () (and power (* (and (or mul div) power))))
(defrule sum () (and product (* (and (or add sub) product))))

;; Parsing the same expression again, the result is now much simpler.
(parseq 'sum "((12 - 3) * 4) ^ (1 / (5 + -3))")

;; Result of the previous expression:
;;
;; ((((((((("12" NIL) NIL) ((- (("3" NIL) NIL)))) NIL) ((* ("4" NIL)))) NIL)
;;    (^
;;     (((("1" NIL) ((/ (((("5" NIL) NIL) ((+ (("-3" NIL) NIL)))) NIL)))) NIL)
;;      NIL)))
;;   NIL)
;;  NIL)
;;
;; The characters have  been converted to strings (e.g. '#\1  #\2' is now "12")
;; and symbols ('#\+' is now '+').  Whitespace and bracket characters have been
;; eliminated.  The expression  is  still  full of  brackets  (but not  bracket
;; characters),  because lisp  uses  brackets  for the  tree  structure of  the
;; result.  The nesting  of the  tree structure  is reduced  because the  rules
;; 'value', 'bracket' and 'binary' each  remove one level with their processing
;; options.



;; Example 3a - Advanced transformations --------------------------------------

;; Helper function that  turns  '(a ((b c) (d e) ...))' into  '(a b c d e ...)'
;; and '(a nil)' into 'a'.
(defun helper-1 (a rest)
  (if rest (apply #'concatenate `(list (,a) ,@rest)) a))
;; Helper function  that turns  '(a (b c))' into  '(a b c)' and  '(a nil)' into
;; 'a'.
(defun helper-2 (a bc)
  (if bc (helper-1 a (list bc)) a))

(defrule space () (* #\Space))
;; Convert  the string  to an  integer  using common lisp's  built-in  function
;; 'parse-integer'.
(defrule value () (and (? #\-) (+ (char "0-9"))) (:string) (:function #'parse-integer))
(defrule bracket () (and #\( sum #\)) (:choose 1))
(defrule term () (or value bracket))
(defrule binary (symb) (and space symb space) (:choose 1))
(defrule add () (binary #\+) (:constant '+))
(defrule sub () (binary #\-) (:constant '-))
(defrule mul () (binary #\*) (:constant '*))
(defrule div () (binary #\/) (:constant '/))
(defrule exp () (binary #\^) (:constant '^))
;; Use a helper function to tranform the result of the 'power' rule. This turns
;; '(a (^ b))' into '(a ^ b)' and '(a NIL)' into 'a'.
(defrule power () (and term (? (and exp power))) (:function #'helper-2))
;; Use another helper  function to tranform the result of  'product' and 'sum'.
;; For example, this turns '(a (+ b) (-c))' into '(a + b - c)'.
(defrule product () (and power (* (and (or mul div) power))) (:function #'helper-1))
(defrule sum () (and product (* (and (or add sub) product))) (:function #'helper-1))

;; Parsing the same expression again.
(parseq 'sum "((12 - 3) * 4) ^ (1 / (5 + -3))")

;; The result is now  '(((12 - 3) * 4) ^  (1 / (5 + -3)))' but  this time it is
;; not a string  anymore but a list  of lisp integers and symbols!  This is not
;; particularly useful, but looks nice - at least to humans.



;; Example 3b - Transform expression into a lisp form (compiler) --------------

;; Note that example 3b is based on example 2 and not example 3a! The reason is
;; that lisp's syntax is different from normal mathematical syntax.

(defrule space () (* #\Space))
;; Convert  the string  to an  integer  using common lisp's  built-in  function
;; 'parse-integer'.
(defrule value () (and (? #\-) (+ (char "0-9"))) (:string) (:function #'parse-integer))
(defrule bracket () (and #\( sum #\)) (:choose 1))
(defrule term () (or value bracket))
(defrule binary (symb) (and space symb space) (:choose 1))
(defrule add () (binary #\+) (:constant '+))
(defrule sub () (binary #\-) (:constant '-))
(defrule mul () (binary #\*) (:constant '*))
(defrule div () (binary #\/) (:constant '/))
(defrule exp () (binary #\^) (:constant '^))
;; Because lisp's syntax is already like a  parse tree, there is not much to do
;; for the 'power', 'product' and 'sum' rules. We essentially only have to make
;; a list  and put  the operator in  front. Of course  we discard  the optional
;; parts if they are not present.
(defrule power () (and term (? (and exp power)))
  (:lambda (term exp+power) (if exp+power `(expt ,term ,(second exp+power)) term)))
(defrule product () (and power (* (and (or mul div) power)))
  (:lambda (first rest) (if rest `(* ,first ,@rest) first)))
(defrule sum () (and product (* (and (or add sub) product)))
  (:lambda (first rest) (if rest `(+ ,first ,@rest) first)))

;; Parsing the  same expression again:
(parseq 'sum "((12 - 3) * 4) ^ (1 / (5 + -3))")

;; This  is the  result:
;;
(EXPT (* (+ 12 (- 3)) (* 4)) (* 1 (/ (+ 5 (+ -3)))))
;;
;; It's a valid lisp expression with  only few extraneous operations that would
;; require a bit more  work to remove. We just turned  human readable code into
;; machine readable code!

;; We can use this to create a lisp macro  if we like to save us from having to
;; write complicated lisp expressions:

(defmacro easy-math (math-string)
  (parseq 'sum math-string :parse-error t))

(easy-math "3+4^2")

;; As an exercise, the reader could change the parsing rules to allow variables
;; in the  math expressions.  That's actually  really easy.  Then, we  could do
;; actual math on variables in lisp by using this macro!



;; Example 3c - Calculate the result while parsing (calculator) ---------------

;; Note that example 3c is based on example 2 and not example 3a or 3b! This is
;; because in  example 3a, the  results from  the 'power', 'product'  and 'sum'
;; rules  are  processed  in  a  way that  is  not  particularly  suitable  for
;; evaluation - the intent was a nice visual representation. We could of course
;; just eval the result of example 3b, but that would be cheating!

(defrule space () (* #\Space))
;; Convert  the string  to an  integer  using common lisp's  built-in  function
;; 'parse-integer'.
(defrule value () (and (? #\-) (+ (char "0-9"))) (:string) (:function #'parse-integer))
(defrule bracket () (and #\( sum #\)) (:choose 1))
(defrule term () (or value bracket))
(defrule binary (symb) (and space symb space) (:choose 1))
(defrule add () (binary #\+) (:constant '+))
(defrule sub () (binary #\-) (:constant '-))
(defrule mul () (binary #\*) (:constant '*))
(defrule div () (binary #\/) (:constant '/))
(defrule exp () (binary #\^) (:constant '^))
;; Use a  lambda function to  calculate the power if  an exponent is  given and
;; return the value of 'term' if there is no exponent.
(defrule power () (and term (? (and exp power)))
  (:lambda (term exp+power)
    (if exp+power (expt term (second exp+power)) term)))
;; The direct output  of 'product' and 'sum'  is something like (a (+  b) (- c)
;; ...). Conveniently,  the symbols '+', '-',  '*' and '/' already  denote lisp
;; functions. Except for the first, the items in the list (e.g. '(+ b)' and '(-
;; c') can therefore be funcalled. The results can then be added together. This
;; is achieve with a lambda function that acts on the parsing result.
(defrule product () (and power (* (and (or mul div) power)))
  (:lambda (first rest) (apply #'* first (mapcar (lambda (ab) (funcall (first ab) (second ab))) rest))))
(defrule sum () (and product (* (and (or add sub) product)))
  (:lambda (first rest) (apply #'+ first (mapcar (lambda (ab) (funcall (first ab) (second ab))) rest))))

;; Parsing the  same expression again,  the result is  now '6.0'. The  value is
;; floating point,  because a square root  was calculated (due to  the exponent
;; being '1/2').
(parseq 'sum "((12 - 3) * 4) ^ (1 / (5 + -3))")

;; As an exercise, the reader could  change the parsing rules to allow floating
;; point numbers  in the  input. In  a second step,  it would  be nice  to have
;; functions such as 'sin(...)' and 'cos(...)' and  so on. That is not too much
;; work, actually.
