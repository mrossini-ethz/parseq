(ql:quickload :utils)
(use-package :utils)

(defparameter *list-parse-rule-table* (make-hash-table))

(defun parse-list (expression list &key start end junk-allowed)
  (declare (ignore start end)) ;; FIXME
  (let ((pos '(0)))
    (multiple-value-bind (result success newpos) (parse-list-internal expression list pos)
      (if (and success (or junk-allowed (l= list (first newpos))))
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
                              (error (format nil "Unknown rule `(~a ...)'." expression)))))))

(defun quoted-symbol-p (x)
  (and (listp x) (l= x 2) (eql (first x) 'quote) (symbolp (second x))))

;; Tree position functions ---------------------------------------------------

(defun treepos-valid (pos tree)
  (when (listp tree)
      (if (l> pos 1)
          (when (l> tree (first pos))
              (treepos-valid (rest pos) (nth (first pos) tree)))
          (l> tree (first pos)))))

(defun treeitem (pos tree)
  (if (l> pos 1)
      (treeitem (rest pos) (nth (first pos) tree))
      (nth (first pos) tree)))

(defun treepos-length (pos tree)
  (if (l> pos 1)
      (treepos-length (rest pos) (nth (first pos) tree))
      (and (listp (nth (first pos) tree)) (list-length (nth (first pos) tree)))))

(defun treepos-step (pos &optional (delta 1))
  (let ((newpos (copy-tree pos)))
    (incf (car (last newpos)) delta)
    newpos))

(defun treepos-copy (pos)
  (copy-tree pos))

;; Expansion helper macros ---------------------------------------------------

(defmacro test-and-advance (test expr pos &optional (inc 1))
  `(with-gensyms (result ret)
     `(if (treepos-valid ,pos ,expr)
          (let ((,result ,,test) (,ret ,,expr))
            (if ,result
                (progn
                  (setf ,,pos (treepos-step ,,pos ,,inc))
                  (values ,ret t))
                (values nil nil)))
          (values nil nil))))

(defmacro try-and-advance (test pos)
  `(with-gensyms (result success newpos)
     `(multiple-value-bind (,result ,success ,newpos) ,,test
        (if ,success
            (progn
              (setf ,,pos (treepos-copy ,newpos))
              (values ,result t))
            (values nil nil)))))

;; Expansion macros --------------------------------------------------

(defmacro with-expansion (((result-var success-var) expr rule pos args) &body body)
  `(multiple-value-bind (,result-var ,success-var) ,(expand-rule expr rule pos args)
     ,@body))

(defmacro with-expansion-success (((result-var success-var) expr rule pos args) then else)
  `(with-expansion ((,result-var ,success-var) ,expr ,rule ,pos ,args)
     (if ,success-var ,then ,else)))

(defmacro with-expansion-failure (((result-var success-var) expr rule pos args) then else)
  `(with-expansion-success ((,result-var ,success-var) ,expr ,rule ,pos ,args) ,else ,then))

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
    ((quoted-symbol-p rule) (test-and-advance `(eql (treeitem ,pos ,expr) ,rule) `(treeitem ,pos ,expr) pos))
    ;; Is a lambda variable
    ((and (symbolp rule) (have rule args)) (test-and-advance `(eql (treeitem ,pos ,expr) (second ,rule)) `(treeitem ,pos ,expr) pos))
    ;; Is the symbol 'symbol'
    ((and (symbolp rule) (eql rule 'symbol)) (test-and-advance `(symbolp (treeitem ,pos ,expr)) `(treeitem, pos, expr) pos))
    ;; Is the symbol 'form'
    ((and (symbolp rule) (eql rule 'form)) (test-and-advance t `(treeitem ,pos, expr) pos))
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
       (let (,list (,oldpos (treepos-copy ,pos)))
         ;; Loop over the rules
         ,@(loop for r in rule for n upfrom 0 collect
                ;; Bind a variable to the result of the rule expansion
                `(with-expansion-success ((,result ,success) ,expr ,r ,pos ,args)
                   ;; Success
                   (appendf ,list ,result)
                   ;; Failure
                   (progn
                     ;; Rewind position
                     (setf ,pos ,oldpos)
                     ;; Return failure
                     (return-from ,block (values nil nil)))))
         ;; Return success
         (values ,list t)))))

(defun expand-not (expr rule pos args)
  (with-gensyms (oldpos result success)
    ;; Save the current position
    `(let ((,oldpos (treepos-copy ,pos)))
       (with-expansion-failure ((,result ,success) ,expr ,rule ,pos ,args)
         ;; Expression failed, which is good (but only if we have not reached the end of expr)
         (if (treepos-valid ,pos ,expr)
             (let ((,result (treeitem ,pos ,expr)))
               ;; Advance the position by one
               (setf ,pos (treepos-step ,pos))
               (values ,result t))
             (values nil nil))
         ;; Expression succeeded, which is bad
         (progn
           ;; Use the variable in order to avoid causing a warning
           ,result
           ;; Roll back the position
           (setf ,pos ,oldpos)
           ;; Return nil
           (values nil nil))))))

(defun expand-* (expr rule pos args)
   (with-gensyms (ret)
     `(values
       (loop for ,ret = (multiple-value-list ,(expand-rule expr rule pos args)) while (second ,ret) collect (first ,ret))
       t)))

(defun expand-+ (expr rule pos args)
  (with-gensyms (result success ret)
    `(with-expansion-success ((,result ,success) ,expr ,rule ,pos ,args)
       (values
        (append (list ,result) (loop for ,ret = (multiple-value-list ,(expand-rule expr rule pos args)) while (second ,ret) collect (first ,ret)))
        t)
       (values nil nil))))

(defun expand-? (expr rule pos args)
  (with-gensyms (result success)
    `(with-expansion ((,result ,success) ,expr ,rule ,pos ,args)
       (values (if ,success ,result nil) t))))

(defun expand-& (expr rule pos args)
  (with-gensyms (oldpos result success)
    `(let ((,oldpos (treepos-copy ,pos)))
       (with-expansion-success ((,result ,success) ,expr ,rule ,pos ,args)
         (progn
           (setf ,pos ,oldpos)
           (values ,result t))
         (values nil nil)))))

(defun expand-! (expr rule pos args)
  (with-gensyms (oldpos result success)
    `(let ((,oldpos (treepos-copy ,pos)))
       (with-expansion-failure ((,result ,success) ,expr ,rule ,pos ,args)
         ;; Failure, which is good (but only if we're not at the end of expr)
         (if (treepos-valid ,pos ,expr)
             (let ((,result (treeitem ,pos ,expr)))
               (values ,result t))
             (values nil nil))
         ;; Success, which is bad
         (progn
           (setf ,pos ,oldpos)
           (values ,result nil))))))

(defun expand-list (expr rule pos args)
  (with-gensyms (result success length)
    `(if (listp (treeitem ,pos ,expr))
         (let ((,length (treepos-length ,pos ,expr)))
           ;; Go into the list
           (appendf ,pos 0)
           (with-expansion-success ((,result ,success) ,expr ,rule ,pos ,args)
             ;; Success
             (if (= (last-1 ,pos) ,length)
                 (progn
                   ;; Step out of the list and increment the position
                   (setf ,pos (treepos-step (butlast ,pos)))
                   (values (list ,result) t))
                 (values nil nil))
             ;; Failure
             (values nil nil)))
         (values nil nil))))

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
    ;; list
    (list (expand-list expr (second rule) pos args))
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

(defun expand-destructure (destruct-lambda result body)
  `(destructuring-bind ,destruct-lambda (mklist ,result) ,@body))

(defun expand-processing-options (result options)
  (if (null options)
      ;; No processing options
      result
      ;; Have processing options
      `(progn
         ;; To avoid a warning about an unused variable
         ;;,result
         ;; Iterate the options. The last option will affect the returned result.
         ,@(loop for opt in options
              ;; Ensure the options are lists of at least one element
              when (or (not (listp opt)) (l< opt 1)) do (error "Invalid processing option!")
              else collect
                (case (first opt)
                  (:constant (second opt))
                  (:lambda (expand-destructure (second opt) result (cddr opt)))
                  (:destructure (expand-destructure (second opt) result (cddr opt)))
                  (:function `(apply ,(second opt) (mklist ,result)))
                  (:identity `(if ,(second opt) ,result))
                  (:flatten `(if (listp ,result) (flatten ,result) ,result))
                  )))))

;; defrule macro --------------------------------------------------------------

(defmacro defrule (name lambda-list expr &body options)
  ;; Creates a lambda function that parses the given grammar rules.
  ;; It then stores the lambda function in the global list *list-parse-rule-table*,
  ;; therefore the rule functions use a namespace separate from everything
  (with-gensyms (x pos oldpos result success)
    ;; Save the lambda function in the namespace table
    `(setf (gethash ',name *list-parse-rule-table*)
           ;; The lambda function that parses according to the given grammar rules
           #'(lambda (,x ,pos ,@lambda-list)
               ;; Save the previous parsing position and get the parsing result
               (let ((,oldpos (treepos-copy ,pos)))
                 (with-expansion-success ((,result ,success) ,x ,expr ,pos ,lambda-list)
                   ;; Return the parsing result, the success and the new position
                   (values ,(expand-processing-options result options) t ,pos)
                   ;; Return nil as parsing result, failure and the old position
                   (values nil nil ,oldpos)))))))

;; Test area ------------------------------------------------------------------

(defrule sym () symbol)
(defrule form () form)
(defrule and () (and 'a 'b 'c))
(defrule or () (or 'a 'b 'c))
(defrule not () (not 'a))
(defrule * () (* 'a))
(defrule + () (+ 'a))
(defrule ? () (? 'a))
(defrule & () (& 'a))
(defrule ! () (! 'a))
(defrule list () (list 'a))
(defrule var (x) x)
(defrule nest-or-and () (or (and 'a 'b) (and 'a 'c) (and 'd 'e)))
(defrule nest-and-or () (and (or 'a 'b) (or 'a 'c) (or 'd 'e)))
(defrule nest-*-and () (* (and 'a 'b)))
(defrule nest-and-* () (and (* 'a) (* 'b)))
(defrule nest-+-and () (+ (and 'a 'b)))
(defrule nest-and-+ () (and (+ 'a) (+ 'b)))
(defrule nest-list-list () (list (list 'a)))

(defrule loop-name () (and 'named symbol))
(defrule loop-iteration-with () (and 'with symbol '= form (* (and 'and symbol '= form))))
(defrule loop-iteration-up () (and (? (and (or 'from 'upfrom) form)) (? (and (or 'upto 'to 'below) form))))
(defrule loop-iteration-down1 () (and 'from form (or 'downto 'above) form))
(defrule loop-iteration-down2 () (and 'downfrom form (? (and (or 'downto 'to 'above) form))))
(defrule loop-iteration-numeric () (and (or loop-iteration-down1 loop-iteration-down2 loop-iteration-up) (? (and 'by form))))
(defrule loop-iteration-list () (and (or 'in 'on) form (? (and 'by form))))
(defrule loop-iteration-flex () (and '= form (? (and 'then form))))
(defrule loop-iteration-vector () (and 'across form))
(defrule loop-iteration-key () (and (or 'hash-key 'hash-keys) (or 'of 'in) form (? (and 'using (list (and 'hash-value symbol))))))
(defrule loop-iteration-value () (and (or 'hash-value 'hash-values) (or 'of 'in) form (? (and 'using (list (and 'hash-key symbol))))))
(defrule loop-iteration-package () (and (or 'symbol 'symbols 'present-symbol 'present-symbols 'external-symbol 'external-symbols) (or 'of 'in) form))
(defrule loop-iteration-hash () (and 'being (or 'the 'each) (or loop-iteration-key loop-iteration-value loop-iteration-package)))
(defrule loop-iteration-for-body () (or loop-iteration-list loop-iteration-flex loop-iteration-vector loop-iteration-hash loop-iteration-numeric))
(defrule loop-iteration-for () (and (or 'for 'as) symbol loop-iteration-for-body (* (and 'and symbol loop-iteration-for-body))))
(defrule loop-iteration () (or loop-iteration-with loop-iteration-for))
(defrule loop-around () (and (or 'initially 'finally) (+ (not loop-post-iteration))))
(defrule loop-repeat () (and 'repeat form))
(defrule loop-test () (and (or 'while 'until 'always 'never 'thereis) form))
(defrule loop-control () (or loop-around loop-repeat loop-test))
(defrule loop-do () (and (or 'do 'doing) (+ (not loop-post-iteration))))
(defrule loop-condition-and () (and loop-action (* (and 'and loop-action))))
(defrule loop-condition () (and (or 'if 'when 'unless) form loop-condition-and (? (and 'else loop-condition-and)) (? 'end)))
(defrule loop-return () (and 'return (or 'it form)))
(defrule loop-collect () (and (or 'collect 'collecting 'append 'appending 'nconc 'nconcing) (or 'it form) (? (and 'into form))))
(defrule loop-stat () (and (or 'count 'counting 'sum 'summing 'maximize 'maximizing 'minimize 'minimizing) (or 'it form) (? (and 'into form))))
(defrule loop-action () (or loop-do loop-condition loop-return loop-collect loop-stat)) ;; +
(defrule loop-post-iteration () (or loop-control loop-action))
(defrule loop () (and (? loop-name) (* loop-iteration) (* loop-post-iteration)))

(defun test-parse-list (expression list &optional success (result nil result-p) junk-allowed)
  (multiple-value-bind (rslt success-p) (parse-list expression list :junk-allowed junk-allowed)
    (and (xnor success success-p) (or (not result-p) (equal rslt result)))))

(define-test symbol-test ()
  (check
    (test-parse-list 'sym '(a) t 'a)
    (test-parse-list 'sym '((a)) nil nil)
    (test-parse-list 'sym '(1) nil nil)))

(define-test form-test ()
  (check
    (test-parse-list 'form '(a) t 'a)
    (test-parse-list 'form '((a)) t '(a))
    (test-parse-list 'form '(1) t 1)))

(define-test and-test ()
  (check
    ;; (and 'a 'b 'c)
    (test-parse-list 'and '(a b c) t '(a b c))
    (test-parse-list 'and '(a b) nil nil)
    (test-parse-list 'and '(a c) nil nil)
    (test-parse-list 'and '(a) nil nil)))

(define-test or-test ()
  (check
    ;; (or 'a 'b 'c)
    (test-parse-list 'or '(a) t 'a)
    (test-parse-list 'or '(b) t 'b)
    (test-parse-list 'or '(c) t 'c)
    (test-parse-list 'or '(d) nil nil)))

(define-test not-test ()
  (check
    ;; (not 'a)
    (test-parse-list 'not '() nil nil)
    (test-parse-list 'not '(a) nil nil)
    (test-parse-list 'not '(b) t 'b)))

(define-test *-test ()
  (check
    ;; (* 'a)
    (test-parse-list '* '() t nil)
    (test-parse-list '* '(a) t '(a))
    (test-parse-list '* '(a a) t '(a a))
    (test-parse-list '* '(a a a) t '(a a a))
    (test-parse-list '* '(b) nil nil)
    (test-parse-list '* '(b) t nil t)
    (test-parse-list '* '(a b) nil nil)
    (test-parse-list '* '(a b) t '(a) t)))

(define-test +-test ()
  (check
    ;; (+ 'a)
    (test-parse-list '+ '() nil nil)
    (test-parse-list '+ '(a) t '(a))
    (test-parse-list '+ '(a a) t '(a a))
    (test-parse-list '+ '(a a a) t '(a a a))
    (test-parse-list '+ '(b) nil nil)
    (test-parse-list '+ '(a b) nil nil)
    (test-parse-list '+ '(a b) t '(a) t)))

(define-test ?-test ()
  (check
    ;; (? 'a)
    (test-parse-list '? '() t nil)
    (test-parse-list '? '(a) t 'a)
    (test-parse-list '? '(b) t nil t)))

(define-test &-test ()
  (check
    ;; (& 'a)
    (test-parse-list '& '(a) t 'a t)
    (test-parse-list '& '(b) nil nil t)))

(define-test !-test ()
  (check
    ;; (! 'a)
    (test-parse-list '! '() nil nil)
    (test-parse-list '! '(a) nil nil)
    (test-parse-list '! '(b) t 'b t)))

(define-test list-test ()
  (check
    ;; (list 'a)
    (test-parse-list 'list '() nil nil)
    (test-parse-list 'list '(a) nil nil)
    (test-parse-list 'list '((a)) t '(a))
    (test-parse-list 'list '((b)) nil nil)))

(define-test var-test ()
  (check
    (test-parse-list '(var 'a) '(a) t)
    (test-parse-list '(var 'a) '(b) nil)))

(define-test nesting-test ()
  (check
    ;; (or (and 'a 'b) (and 'a 'c) (and 'd 'e))
    (test-parse-list 'nest-or-and '(a) nil nil)
    (test-parse-list 'nest-or-and '(a b) t '(a b))
    (test-parse-list 'nest-or-and '(a c) t '(a c))
    (test-parse-list 'nest-or-and '(a e) nil nil)
    (test-parse-list 'nest-or-and '(d) nil nil)
    (test-parse-list 'nest-or-and '(d e) t '(d e))

    ;; (and (or 'a 'b) (or 'a 'c) (or 'd 'e))
    (test-parse-list 'nest-and-or '(a) nil nil)
    (test-parse-list 'nest-and-or '(b) nil nil)
    (test-parse-list 'nest-and-or '(a a) nil nil)
    (test-parse-list 'nest-and-or '(b a) nil nil)
    (test-parse-list 'nest-and-or '(a c) nil nil)
    (test-parse-list 'nest-and-or '(b c) nil nil)
    (test-parse-list 'nest-and-or '(a a d) t '(a a d))
    (test-parse-list 'nest-and-or '(b a d) t '(b a d))
    (test-parse-list 'nest-and-or '(a c d) t '(a c d))
    (test-parse-list 'nest-and-or '(b c d) t '(b c d))
    (test-parse-list 'nest-and-or '(a a e) t '(a a e))
    (test-parse-list 'nest-and-or '(b a e) t '(b a e))
    (test-parse-list 'nest-and-or '(a c e) t '(a c e))
    (test-parse-list 'nest-and-or '(b c e) t '(b c e))

    ;; (* (and 'a 'b))
    (test-parse-list 'nest-*-and '() t nil)
    (test-parse-list 'nest-*-and '(a) nil nil)
    (test-parse-list 'nest-*-and '(a) t nil t)
    (test-parse-list 'nest-*-and '(a b) t)
    (test-parse-list 'nest-*-and '(a b a) nil nil)
    (test-parse-list 'nest-*-and '(a b a) t '((a b)) t)
    (test-parse-list 'nest-*-and '(a b a b) t)

    ;; (and (* 'a 'b))
    (test-parse-list 'nest-and-* '() t '(nil nil))
    (test-parse-list 'nest-and-* '(a) t '((a) nil))
    (test-parse-list 'nest-and-* '(a a) t '((a a) nil))
    (test-parse-list 'nest-and-* '(b) t '(nil (b)))
    (test-parse-list 'nest-and-* '(b b) t '(nil (b b)))
    (test-parse-list 'nest-and-* '(a b) t '((a) (b)))
    (test-parse-list 'nest-and-* '(a a b) t '((a a) (b)))
    (test-parse-list 'nest-and-* '(a b b) t '((a) (b b)))

    ;; (+ (and 'a 'b))
    (test-parse-list 'nest-+-and '() nil nil)
    (test-parse-list 'nest-+-and '(a) nil nil)
    (test-parse-list 'nest-+-and '(a b) t '((a b)))
    (test-parse-list 'nest-+-and '(a b a) nil)
    (test-parse-list 'nest-+-and '(a b a) t '((a b)) t)
    (test-parse-list 'nest-+-and '(a b a b) t '((a b) (a b)))

    ;; (and (+ 'a 'b))
    (test-parse-list 'nest-and-+ '() nil nil)
    (test-parse-list 'nest-and-+ '(a) nil nil)
    (test-parse-list 'nest-and-+ '(a a) nil nil)
    (test-parse-list 'nest-and-+ '(b) nil nil)
    (test-parse-list 'nest-and-+ '(b b) nil nil)
    (test-parse-list 'nest-and-+ '(a b) t '((a) (b)))
    (test-parse-list 'nest-and-+ '(a a b) t '((a a) (b)))
    (test-parse-list 'nest-and-+ '(a b b) t '((a) (b b)))

    (test-parse-list 'nest-list-list '(((a))) t '((a)))
))

(define-test loop-test ()
  (check
    (test-parse-list 'loop '(named q for a from 0 below 10 by 10) t)
    (test-parse-list 'loop '(named q for a from 0 above -10 by 10) t)
    (test-parse-list 'loop '(named q for a downfrom 0 above 10 by 10) t)
    (test-parse-list 'loop '(named q for a in lst by #'cdr) t)
    (test-parse-list 'loop '(named q for a = 0 then (1+ a)) t)
    (test-parse-list 'loop '(named q for a across vec) t)
    (test-parse-list 'loop '(named q for k being the hash-key of (hsh) using (hash-value v)) t)
    (test-parse-list 'loop '(named q for v being the hash-value of (hsh) using (hash-key v)) t)
    (test-parse-list 'loop '(named q for k being the external-symbol of (pkg)) t)
    (test-parse-list 'loop '(for i in lst initially a b c while d) t)
    (test-parse-list 'loop '(for i in list repeat 5) t)
    (test-parse-list 'loop '(for i in list thereis (> i 0)) t)
    (test-parse-list 'loop '(for i across vec when (> i 0) collecting it into q and summing i into n else maximize i into m and return 5 end) t)
    (test-parse-list 'loop '(for i across vec unless (minusp i) count i into q) t)
))

(define-test parse-list-test ()
  (check
    (symbol-test)
    (form-test)
    (and-test)
    (or-test)
    (not-test)
    (*-test)
    (+-test)
    (?-test)
    (&-test)
    (!-test)
    (list-test)
    (var-test)
    (nesting-test)
    (loop-test)
))

(parse-list-test)
