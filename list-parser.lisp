(in-package :parser)

(defparameter *list-parse-rule-table* (make-hash-table))

(defun parse-list (expression list &key start end junk-allowed)
  (declare (ignore start end)) ;; FIXME
  (let ((pos '(0)))
    (multiple-value-bind (result success newpos) (parse-list-internal expression list pos)
      (if (and success (or junk-allowed (= (length list) (first newpos))))
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
  (cond
    ((listp tree)
     (if (l> pos 1)
         (when (l> tree (first pos))
           (treepos-valid (rest pos) (nth (first pos) tree)))
         (l> tree (first pos))))
    (t (< (first pos) (length tree)))))

(defun treeitem (pos tree)
  (cond
    ((listp tree)
     (if (l> pos 1)
         (treeitem (rest pos) (nth (first pos) tree))
         (nth (first pos) tree)))
    (t (elt tree (first pos)))))

(defun treepos-length (pos tree)
  (cond
    ((listp tree)
     (if (l> pos 1)
         (treepos-length (rest pos) (nth (first pos) tree))
         (and (listp (nth (first pos) tree)) (list-length (nth (first pos) tree)))))
    (t 1)))

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
    ((quoted-symbol-p rule) (test-and-advance `(symbol= (treeitem ,pos ,expr) ,rule) `(treeitem ,pos ,expr) pos))
    ;; Is a character
    ((characterp rule) (test-and-advance `(char= (treeitem ,pos ,expr) ,rule) `(treeitem ,pos ,expr) pos))
    ;; Is a character string
    ((stringp rule) (test-and-advance `(string= ,expr ,rule :start1 (first ,pos) :end1 (+ (first ,pos) (length ,rule))) `(subseq ,expr (first ,pos) (+ (first ,pos) (length ,rule))) pos (length rule)))
    ;; Is a lambda variable
    ((and (symbolp rule) (have rule args)) (test-and-advance `(symbol= (treeitem ,pos ,expr) (second ,rule)) `(treeitem ,pos ,expr) pos))
    ;; Is the symbol 'symbol'
    ((and (symbolp rule) (symbol= rule 'symbol)) (test-and-advance `(symbolp (treeitem ,pos ,expr)) `(treeitem, pos, expr) pos))
    ;; Is the symbol 'form'
    ((and (symbolp rule) (symbol= rule 'form)) (test-and-advance t `(treeitem ,pos, expr) pos))
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
  (case-test ((first rule) :test symbol=)
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
  (let ((procs (loop for opt in options
                  ;; Ensure the options are lists of at least one element
                  when (or (not (listp opt)) (l< opt 1)) do (error "Invalid processing option!")
                  when (have (first opt) '(:constant :lambda :destructure :function :identity :flatten))
                             collect opt)))
    (if (null procs)
        result
        `(progn
           ;; To avoid a warning about an unused variable
           ,result
           ;; Execute the procs in order
           ,@(loop for opt in procs collect
                  (case (first opt)
                    (:constant (second opt))
                    (:lambda (expand-destructure (second opt) result (cddr opt)))
                    (:destructure (expand-destructure (second opt) result (cddr opt)))
                    (:function `(apply ,(second opt) (mklist ,result)))
                    (:identity `(if ,(second opt) ,result))
                    (:flatten `(if (listp ,result) (flatten ,result) ,result))
                    ))))))

(defmacro with-special-vars ((&rest vars) &body body)
  `(let (,@vars)
     (declare (special ,@(loop for v in vars collect (if (listp v) (first v) v))))
     ,@body))

(defmacro with-special-vars-from-options (options &body body)
  (let ((bindings (loop for opt in options
                     when (or (not (listp opt)) (l< opt 1)) do (error "Invalid processing option.")
                     when (eql (first opt) :let) append (rest opt))))
    (if bindings
        `(with-special-vars (,@bindings) ,@body)
        `(progn ,@body))))

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
               (declare (special ,@(loop for opt in options when (eql (first opt) :external) append (rest opt))))
               (with-special-vars-from-options ,options
                 ;; Save the previous parsing position and get the parsing result
                 (let ((,oldpos (treepos-copy ,pos)))
                   (with-expansion-success ((,result ,success) ,x ,expr ,pos ,lambda-list)
                     ;; Return the parsing result, the success and the new position
                     (values ,(expand-processing-options result options) t ,pos)
                     ;; Return nil as parsing result, failure and the old position
                     (values nil nil ,oldpos))))))))

;; Namespace macros -----------------------------------------------------------

(defmacro with-local-rules (&body body)
  ;; Shadow the global rule table with a new hash table
  `(let ((*list-parse-rule-table* (make-hash-table)))
     ;; Execute the body
     ,@body))

;; Test area ------------------------------------------------------------------

