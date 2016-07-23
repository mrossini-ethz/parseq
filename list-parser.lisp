(defparameter *list-parse-rule-table* (make-hash-table))

(defun parse-list (expression rule &optional (pos 0))
  (cond
    ;; Rule is nil
    ((null rule) (null expression))
    ;; Rule is a named rule (without args)
    ((symbolp rule) (let ((fun (gethash rule *list-parse-rule-table*)))
                      (if fun
                          (funcall fun expression pos)
                          (error "Unknown rule."))))
    ((listp rule) (let ((fun (gethash (first rule) *list-parse-rule-table*)))
                    (if fun
                        (apply fun expression pos (rest rule))
                        (error "Unknown rule."))))
    ))

(defun quoted-symbol-p (x)
  (and (listp x) (l= x 2) (eql (first x) 'quote) (symbolp (second x))))

(defun expand-atom (expr rule pos &rest args)
  (cond
    ;; Is a quoted symbol
    ((quoted-symbol-p rule) `(if (eql (nth ,pos ,expr) ',(second rule)) ',(second rule)))
    ;; Is a lambda variable
    ((and (symbolp rule) (have rule args)) `(if (eql (nth ,pos ,expr) ,rule) ,rule))
    ;; Is a call to another rule (without args)
    ((symbolp rule) `(parse-list ,expr ',rule ,pos))
    ))

(defun expand-or (expr rule pos)
  `(cond
     ,@(loop for r in rule collect (list (expand-rule expr r pos)))))

(defun expand-and (expr rule pos)
  ;; Create gensyms for the list of results, the individual result and the block to return from when short-circuiting
  (with-gensyms (list result block)
    ;; Block to return from when short-circuiting
    `(block ,block
       ;; Initialize the list of results
       (let (,list)
         ;; Loop over the rules
         ,@(loop for r in rule for n upfrom 0 collect
                ;; Bind a variable to the result of the rule expansion
                `(let ((,result ,(expand-rule expr r `(+ ,pos ,n))))
                   ;; If the result is nil, return nil
                   (unless ,result
                     (return-from ,block nil))
                   ;; Otherwise append the result
                   (appendf ,list ,result)))))))

(defun expand-rule (expr rule pos &rest args)
  ;; Rule is
  (cond
    ;; ... nil
    ((null rule) (expand-atom expr nil pos))
    ;; ... an atom
    ((atom rule) (apply #'expand-atom expr rule pos args))
    ;; ... a quoted symbol
    ((quoted-symbol-p rule) (expand-atom expr rule pos))
    ;; ... an OR expression
    ((eql 'or (first rule)) (expand-or expr (rest rule) pos))
    ;; ... an AND expression
    ((eql 'and (first rule)) (expand-and expr (rest rule) pos))
    ;; ... a call to another rule (with args)
    ((symbolp (first rule)) (parse-list (rest expr) rule pos))
    ))

(defmacro defrule (name lambda-list expr)
  (with-gensyms (x pos)
    `(setf (gethash ',name *list-parse-rule-table*)
           #'(lambda (,x ,pos ,@lambda-list)
               ,(apply #'expand-rule x expr pos lambda-list)))))

(defrule hello-world () (and 'hello 'world))
(defrule hey-you () (and 'hey 'you))
(defrule test () (or hey-you hello-world))

(parse-list '(hey you) 'test)
