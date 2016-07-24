(ql:quickload :utils)
(use-package :utils)

(defparameter *list-parse-rule-table* (make-hash-table))

(defun parse-list (expression list &key start end junk-allowed)
  (declare (ignore start end)) ;; FIXME
  (let ((pos 0))
    (multiple-value-bind (result success newpos) (parse-list-internal expression list pos)
      (if (and success (or junk-allowed (l= list newpos)))
          (values result t)
          (values nil nil)))))

(defun parse-list-internal (expression list pos)
  (cond
    ;; Expression is nil
    ((null expression) (null list))
    ;; Expression is a named rule (without args)
    ((symbolp expression) (let ((fun (gethash expression *list-parse-rule-table*)))
                            (if fun
                                (funcall fun list pos)
                                (error (format nil "Unknown rule `~a'." expression)))))
    ;; Expression is a named rule (with args)
    ((listp expression) (let ((fun (gethash (first expression) *list-parse-rule-table*)))
                          (if fun
                              (apply fun list pos (rest expression))
                              (error "Unknown rule."))))))

(defun quoted-symbol-p (x)
  (and (listp x) (l= x 2) (eql (first x) 'quote) (symbolp (second x))))

;; Expansion helper macros ---------------------------------------------------

(defmacro test-and-advance (test expr pos &optional (inc 1))
  `(with-gensyms (result ret)
     `(let ((,result ,,test) (,ret ,,expr))
        (if ,result
            (progn
              (incf ,,pos ,,inc)
              (values ,ret t))
            (values nil nil)))))
            

(defmacro try-and-advance (test pos)
  `(with-gensyms (result success newpos)
     `(multiple-value-bind (,result ,success ,newpos) ,,test
        (if ,success
            (progn
              (setf ,,pos ,newpos)
              (values ,result t))
            (values nil nil)))))

;; Expansion functions -----------------------------------------------

;; These are helper functions for the defrule macro.
;; Therefore, the functions contain macro code and need to be treated as such.
;; All take the list that should be parsed as `expr', the parsing `rule',
;; the current `pos'ition in the list as well as the `arg'uments to the defrule.
;; The intent is to generate lisp code for parsing.
;; They return two values: The portion of the `expr' that was parsed, and a success value

(defun expand-atom (expr rule pos args)
  (cond
    ;; Is a quoted symbol
    ((quoted-symbol-p rule) (test-and-advance `(eql (nth ,pos ,expr) ,rule) `(nth ,pos ,expr) pos))
    ;; Is a lambda variable
    ((and (symbolp rule) (have rule args)) (test-and-advance `(eql (nth ,pos ,expr) (second ,rule)) `(nth ,pos ,expr) pos))
    ;; Is a call to another rule (without args)
    ((symbolp rule) (try-and-advance `(parse-list-internal ',rule ,expr ,pos) pos))
    ))

(defmacro cond-or (&rest clauses)
  ;; Helper macro for expand-or. Works like cond, but operates on the two values returned by each clause.
  (if clauses
      (with-gensyms (result success)
        `(multiple-value-bind (,result ,success) ,(first clauses)
           (if ,success
               (values ,result t)
               (cond-or ,@(rest clauses)))))
      `(values nil nil)))

(defun expand-or (expr rule pos args)
  `(cond-or ,@(loop for r in rule collect (expand-rule expr r pos args))))

(defun expand-and (expr rule pos args)
  ;; Create gensyms for the list of results, the individual result and the block to return from when short-circuiting
  (with-gensyms (list result block oldpos success)
    ;; Block to return from when short-circuiting
    `(block ,block
       ;; Initialize the list of results
       (let (,list (,oldpos ,pos))
         ;; Loop over the rules
         ,@(loop for r in rule for n upfrom 0 collect
                ;; Bind a variable to the result of the rule expansion
                `(multiple-value-bind (,result ,success) ,(expand-rule expr r pos args)
                   ;; If success ...
                   (unless ,success
                     ;; Rewind position
                     (setf ,pos ,oldpos)
                     ;; Return failure
                     (return-from ,block (values nil nil)))
                   ;; Otherwise append the result
                   (appendf ,list ,result)))
         ;; Return success
         (values ,list t)))))

(defun expand-not (expr rule pos args)
  (with-gensyms (oldpos result success)
    ;; Save the current position
    `(let ((,oldpos ,pos))
       (multiple-value-bind (,result ,success) ,(expand-rule expr rule pos args)
       ;; If the rule ...
       (if ,success
           ;; is successful (which is bad)
           (progn
             ;; Roll back the position
             (setf ,pos ,oldpos)
             ;; Return nil
             (values nil nil))
           ;; fails
           (let ((,result (nth ,pos ,expr)))
             ;; Advance the position by one
             (incf ,pos)
             (values ,result t)))))))

(defun expand-* (expr rule pos args)
   (with-gensyms (ret)
     `(values 
       (loop for ,ret = (multiple-value-list ,(expand-rule expr rule pos args)) while (second ,ret) collect (first ,ret))
       t)))

(defun expand-+ (expr rule pos args)
  (with-gensyms (result success ret)
    `(multiple-value-bind (,result ,success) ,(expand-rule expr rule pos args)
       (if ,success
           (values
            (append (list ,result) (loop for ,ret = (multiple-value-list ,(expand-rule expr rule pos args)) while (second ,ret) collect (first ,ret)))
            t)))))

(defun expand-? (expr rule pos args)
  (with-gensyms (result success)
    `(multiple-value-bind (,result ,success) ,(expand-rule expr rule pos args)
       (values (if ,success ,result nil) t))))

(defun expand-& (expr rule pos args)
  (with-gensyms (oldpos result success)
    `(let ((,oldpos ,pos))
       (multiple-value-bind (,result ,success) ,(expand-rule expr rule pos args)
         (if ,success
             (progn
               (setf ,pos ,oldpos)
               (values ,result t))
             (values nil nil))))))

(defun expand-! (expr rule pos args)
  (with-gensyms (oldpos result success)
    `(let ((,oldpos ,pos))
       (multiple-value-bind (,result ,success) ,(expand-rule expr rule pos args)
         (if ,success
             (progn
               (setf ,pos ,oldpos)
               (values ,result nil))
             (values nil t))))))

(defun expand-parse-call (expr rule pos args)
  ;; Makes a call to `parse-list-internal' with or without quoting the rule arguments depending on whether they are arguments to the current rule
  `(parse-list-internal `(,,@(loop for r in rule for n upfrom 0 collect (if (and (plusp n) (have r args)) r `(quote ,r)))) ,expr ,pos))

(defun expand-list-expr (expr rule pos args)
  ;; Rule is a ...
  (case (first rule)
    ;; ordered choice
    (or (expand-or expr (rest rule) pos args))
    ;; sequence
    (and (expand-and expr (rest rule) pos args))
    ;; negation
    (not (expand-not expr (second rule) pos args))
    ;; greedy repetition
    (* (expand-* expr (second rule) pos args))
    ;; greedy positive repetition
    (+ (expand-+ expr (second rule) pos args))
    ;; optional
    (? (expand-? expr (second rule) pos args))
    ;; followed-by predicate
    (& (expand-& expr (second rule) pos args))
    ;; not-followed-by predicate
    (! (expand-! expr (second rule) pos args))
    ;; a call to another rule (with args)
    (t (try-and-advance (expand-parse-call expr rule pos args) pos))))

(defun expand-rule (expr rule pos args)
  ;; Rule is
  (cond
    ;; ... nil
    ((null rule) (expand-atom expr nil pos nil))
    ;; ... an atom
    ((atom rule) (expand-atom expr rule pos args))
    ;; ... a quoted symbol
    ((quoted-symbol-p rule) (expand-atom expr rule pos args))
    ;; ... a list expression
    (t (expand-list-expr expr rule pos args))))

;; defrule macro --------------------------------------------------------------

(defmacro defrule (name lambda-list expr)
  ;; Creates a lambda function that parses the given grammar rules.
  ;; It then stores the lambda function in the global list *list-parse-rule-table*,
  ;; therefore the rule functions use a namespace separate from everything
  (with-gensyms (x pos oldpos result success)
    ;; Save the lambda function in the namespace table
    `(setf (gethash ',name *list-parse-rule-table*)
           ;; The lambda function that parses according to the given grammar rules
           #'(lambda (,x ,pos ,@lambda-list)
               ;; Save the previous parsing position and get the parsing result
               (let ((,oldpos ,pos))
                 (multiple-value-bind (,result ,success) ,(expand-rule x expr pos lambda-list)
                   ;; If parsing was successful ...
                   (if ,success
                       ;; Return the parsing result, the success and the new position
                       (values ,result t ,pos)
                       ;; Return nil as parsing result, failure and the old position
                       (values nil nil ,oldpos))))))))

;; Tryout area ----------------------------------------------------------------

(defrule hello-world () (and 'hello 'world))
(defrule hey-you () (and 'hey 'you))
(defrule hey-x (x) (and 'hey x))
(defrule test () (or hey-you hello-world))
(defrule test-x (x) (or (hey-x x) hello-world))
(defrule test-y () (or (hey-x 'y) hello-world))
(defrule test-or-and () (or (and 'hello 'you) (and 'hello 'world)))

(parse-list 'test '(hello you))
(parse-list 'test '(hello world))
(parse-list '(hey-x 'yo) '(hey yo))
(parse-list '(test-x 'w) '(hey w))
(parse-list '(test-x 'w) '(hello world))
(parse-list 'test-y '(hey y))
(parse-list 'test-y '(hello world))
(parse-list 'test-or-and '(hello you))
(parse-list 'test-or-and '(hello world))

;; Test area ------------------------------------------------------------------

(defrule sym () 'a)
(defrule and () (and 'a 'b 'c))
(defrule or () (or 'a 'b 'c))
(defrule not () (not 'a))
(defrule * () (* 'a))
(defrule + () (+ 'a))
(defrule ? () (? 'a))
(defrule & () (& 'a))
(defrule ! () (! 'a))
(defrule var (x) x)
(defrule nest-or-and () (or (and 'a 'b) (and 'a 'c) (and 'd 'e)))
(defrule nest-and-or () (and (or 'a 'b) (or 'a 'c) (or 'd 'e)))
(defrule nest-*-and () (* (and 'a 'b)))
(defrule nest-and-* () (and (* 'a) (* 'b)))
(defrule nest-+-and () (+ (and 'a 'b)))
(defrule nest-and-+ () (and (+ 'a) (+ 'b)))

(defun test-parse-list (expression list success)
  (multiple-value-bind (result success-p pos) (parse-list expression list)
    (declare (ignore result pos))
    (xnor success success-p)))

(define-test symbol-test ()
  (check
    (test-parse-list 'sym '(a) t)
    (test-parse-list 'sym '(b) nil)))

(define-test and-test ()
  (check
    (test-parse-list 'and '(a b c) t)
    (test-parse-list 'and '(a b) nil)
    (test-parse-list 'and '(a c) nil)
    (test-parse-list 'and '(a) nil)))

(define-test or-test ()
  (check
    (test-parse-list 'or '(a) t)
    (test-parse-list 'or '(b) t)
    (test-parse-list 'or '(c) t)
    (test-parse-list 'or '(d) nil)))

(define-test not-test ()
  (check
    (test-parse-list 'not '(a) nil)
    (test-parse-list 'not '(b) t)
    (test-parse-list 'not '(c) t)
    (test-parse-list 'not '(d) t)))

(define-test *-test ()
  (check
    (test-parse-list '* '() t)
    (test-parse-list '* '(a) t)
    (test-parse-list '* '(a a) t)
    (test-parse-list '* '(a a a) t)
    (test-parse-list '* '(b) nil)
    (test-parse-list '* '(a b) nil)))

(define-test +-test ()
  (check
    (test-parse-list '+ '() nil)
    (test-parse-list '+ '(a) t)
    (test-parse-list '+ '(a a) t)
    (test-parse-list '+ '(a a a) t)
    (test-parse-list '+ '(b) nil)
    (test-parse-list '+ '(a b) nil)))

(define-test ?-test ()
  (check
    (test-parse-list '? '() t)
    (test-parse-list '? '(a) t)
    (test-parse-list '? '(b) nil)))

(define-test &-test ()
  (check
    (test-parse-list '& '(a) nil)
    (test-parse-list '& '(b) nil)))

(define-test !-test ()
  (check
    (test-parse-list '! '(a) nil)
    (test-parse-list '! '(b) nil)))

(define-test var-test ()
  (check
    (test-parse-list '(var 'a) '(a) t)
    (test-parse-list '(var 'a) '(b) nil)))

(define-test nesting-test ()
  (check
    (test-parse-list 'nest-or-and '(a) nil)
    (test-parse-list 'nest-or-and '(a b) t)
    (test-parse-list 'nest-or-and '(a c) t)
    (test-parse-list 'nest-or-and '(a e) nil)
    (test-parse-list 'nest-or-and '(d) nil)
    (test-parse-list 'nest-or-and '(d e) t)

    (test-parse-list 'nest-and-or '(a) nil)
    (test-parse-list 'nest-and-or '(b) nil)
    (test-parse-list 'nest-and-or '(a a) nil)
    (test-parse-list 'nest-and-or '(b a) nil)
    (test-parse-list 'nest-and-or '(a c) nil)
    (test-parse-list 'nest-and-or '(b c) nil)
    (test-parse-list 'nest-and-or '(a a d) t)
    (test-parse-list 'nest-and-or '(b a d) t)
    (test-parse-list 'nest-and-or '(a c d) t)
    (test-parse-list 'nest-and-or '(b c d) t)
    (test-parse-list 'nest-and-or '(a a e) t)
    (test-parse-list 'nest-and-or '(b a e) t)
    (test-parse-list 'nest-and-or '(a c e) t)
    (test-parse-list 'nest-and-or '(b c e) t)

    (test-parse-list 'nest-*-and '() t)
    (test-parse-list 'nest-*-and '(a) nil)
    (test-parse-list 'nest-*-and '(a b) t)
    (test-parse-list 'nest-*-and '(a b a) nil)
    (test-parse-list 'nest-*-and '(a b a b) t)

    (test-parse-list 'nest-and-* '() t)
    (test-parse-list 'nest-and-* '(a) t)
    (test-parse-list 'nest-and-* '(a a) t)
    (test-parse-list 'nest-and-* '(b) t)
    (test-parse-list 'nest-and-* '(b b) t)
    (test-parse-list 'nest-and-* '(a b) t)
    (test-parse-list 'nest-and-* '(a a b) t)
    (test-parse-list 'nest-and-* '(a b b) t)

    (test-parse-list 'nest-+-and '() nil)
    (test-parse-list 'nest-+-and '(a) nil)
    (test-parse-list 'nest-+-and '(a b) t)
    (test-parse-list 'nest-+-and '(a b a) nil)
    (test-parse-list 'nest-+-and '(a b a b) t)

    (test-parse-list 'nest-and-+ '() nil)
    (test-parse-list 'nest-and-+ '(a) nil)
    (test-parse-list 'nest-and-+ '(a a) nil)
    (test-parse-list 'nest-and-+ '(b) nil)
    (test-parse-list 'nest-and-+ '(b b) nil)
    (test-parse-list 'nest-and-+ '(a b) t)
    (test-parse-list 'nest-and-+ '(a a b) t)
    (test-parse-list 'nest-and-+ '(a b b) t)
))

(define-test parse-list-test ()
  (check
    (symbol-test)
    (and-test)
    (or-test)
    (not-test)
    (*-test)
    (+-test)
    (?-test)
    (&-test)
    (!-test)
    (var-test)
    (nesting-test)
))

(parse-list-test)
