(ql:quickload :utils)
(use-package :utils)

(defparameter *list-parse-rule-table* (make-hash-table))

(defun parse-list (expression list &optional (pos 0))
  (cond
    ;; Expression is nil
    ((null expression) (null list))
    ;; Expression is a named rule (without args)
    ((symbolp expression) (let ((fun (gethash expression *list-parse-rule-table*)))
                      (if fun
                          (funcall fun list pos)
                          (error (format nil "Unknown rule `~a'." expression)))))
    ((listp expression) (let ((fun (gethash (first expression) *list-parse-rule-table*)))
                    (if fun
                        (apply fun list pos (rest expression))
                        (error "Unknown rule."))))
    ))

(defun quoted-symbol-p (x)
  (and (listp x) (l= x 2) (eql (first x) 'quote) (symbolp (second x))))

(defmacro test-and-advance (test expr pos &optional (inc 1))
  `(with-gensyms (result ret)
     `(let ((,result ,,test) (,ret ,,expr))
        (when ,result
          (incf ,,pos ,,inc)
          ,ret))))

(defmacro try-and-advance (test pos)
  `(with-gensyms (result success newpos)
     `(multiple-value-bind (,result ,success ,newpos) ,,test
        (when ,success
          (setf ,,pos ,newpos)
          ,result))))

(defun expand-atom (expr rule pos args)
  (cond
    ;; Is a quoted symbol
    ((quoted-symbol-p rule) (test-and-advance `(eql (nth ,pos ,expr) ,rule) `(nth ,pos ,expr) pos))
    ;; Is a lambda variable
    ((and (symbolp rule) (have rule args)) (test-and-advance `(eql (nth ,pos ,expr) (second ,rule)) `(nth ,pos ,expr) pos))
    ;; Is a call to another rule (without args)
    ((symbolp rule) (try-and-advance `(parse-list ',rule ,expr ,pos) pos))
    ))

(defun expand-or (expr rule pos args)
  `(cond
     ,@(loop for r in rule collect (list (expand-rule expr r pos args)))))

(defun expand-and (expr rule pos args)
  ;; Create gensyms for the list of results, the individual result and the block to return from when short-circuiting
  (with-gensyms (list result block)
    ;; Block to return from when short-circuiting
    `(block ,block
       ;; Initialize the list of results
       (let (,list)
         ;; Loop over the rules
         ,@(loop for r in rule for n upfrom 0 collect
                ;; Bind a variable to the result of the rule expansion
                `(let ((,result ,(expand-rule expr r pos args)))
                   ;; If the result is nil, return nil
                   (unless ,result
                     (return-from ,block))
                   ;; Otherwise append the result
                   (appendf ,list ,result)))))))

(defun expand-not (expr rule pos args)
  (with-gensyms (oldpos result)
    ;; Save the current position
    `(let ((,oldpos ,pos))
       ;; If the rule ...
       (if ,(expand-rule expr rule pos args)
           ;; is successful
           (progn
             ;; Roll back the position
             (setf ,pos ,oldpos)
             ;; Return nil
             nil)
           ;; fails
           (let ((,result (nth ,pos ,expr)))
             ;; Advance the position by one
             (incf ,pos)
             ,result)))))

(defun make-parse-call (expr rule pos args)
  ;; Makes a call to `parse-list' with or without quoting the rule arguments depending on whether they are arguments to the current rule
  `(parse-list `(,,@(loop for r in rule for n upfrom 0 collect (if (and (plusp n) (have r args)) r `(quote ,r)))) ,expr ,pos))

(defun expand-list-expr (expr rule pos args)
  ;; Rule is ...
  (case (first rule)
    ;; an OR expression
    (or (expand-or expr (rest rule) pos args))
    ;; an AND expression
    (and (expand-and expr (rest rule) pos args))
    ;; a NOT expression
    (not (expand-not expr (second rule) pos args))
    ;; a call to another rule (with args)
    (t (try-and-advance (make-parse-call expr rule pos args) pos))))

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

(defmacro defrule (name lambda-list expr)
  (with-gensyms (x pos oldpos result)
    `(setf (gethash ',name *list-parse-rule-table*)
           #'(lambda (,x ,pos ,@lambda-list)
               (let ((,oldpos ,pos) (,result ,(expand-rule x expr pos lambda-list)))
                 (if ,result
                   (values ,result t ,pos)
                   (values nil nil ,oldpos)))))))

;; Tryout area ----------------------------------------------------------------

(defrule hello-world () (and 'hello 'world))
(defrule hey-you () (and 'hey 'you))
(defrule hey-x (x) (and 'hey x))
(defrule test () (or hey-you hello-world))
(defrule test-x (x) (or (hey-x x) hello-world))
(defrule test-y () (or (hey-x 'y) hello-world))

(parse-list 'test '(hey you))
(parse-list 'test '(hello world))
(parse-list '(hey-x 'yo) '(hey yo))
(parse-list '(test-x 'w) '(hey w))
(parse-list '(test-x 'w) '(hello world))
(parse-list 'test-y '(hey y))
(parse-list 'test-y '(hello world))

;; Test area ------------------------------------------------------------------

(defrule sym () 'a)
(defrule and () (and 'a 'b 'c))
(defrule or () (or 'a 'b 'c))
(defrule not () (not 'a))
(defrule var (x) x)
(defrule nest () sym)

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

(define-test var-test ()
  (check
    (test-parse-list '(var 'a) '(a) t)
    (test-parse-list '(var 'a) '(b) nil)))

(define-test nesting-test ()
  (check
    (test-parse-list 'nest '(a) t)
    (test-parse-list 'nest '(b) nil)))

(define-test parse-list-test ()
  (check
    (symbol-test)
    (and-test)
    (or-test)
    (not-test)
    (var-test)
    (nesting-test)
))

(parse-list-test)
