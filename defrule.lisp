(in-package :parseq)

;; List of operators used in parsing expressions. Each item is a symbol
;; a matching function and an expansion function
(defparameter *operator-table* nil)

;; List of possible terminals with each item being a list of terminal
;; name, a rule matching function as well as an expansion function.
(defparameter *terminal-table* nil)

;; Hash table that maps the rule names (symbols) to the functions
;; that parse the corresponding rule. Being a special variable,
;; it can be shadowed. This is used in the macro with-local-rules.
(defparameter *rule-table* (make-hash-table))

;; Hash table that will be set up each time (parseq ...) is called
;; (if enabled). Each parse function that is called can store its
;; memoizing table there. This way, the memoizing tables are reset
;; each time (parseq ...) is called.
(defparameter *packrat-table* nil)

;; List of terminals that have already failed at a given position in the
;; sequence. This is used for error reporting. The variable ist rebound
;; in each call of (parseq ...). It will later contain the value
;; (cons <pos> <list-of-terminals>) where <pos> is the rightmost position
;; in the sequence where parsing failed.
(defvar *terminal-failure-list* nil)

(defun parseq (rule sequence &key (start 0) end junk-allowed parse-error)
  ;; Parses sequence according to the given rule. A subsequence
  ;; can be parsed by passing the start and/or end arguments.
  ;; The parse does fails if the end of the sequence is not reached
  ;; unless junk-allowed is given.
  (let ((pos (make-treepos start)) (*terminal-failure-list* (cons (make-treepos) nil)) (*packrat-table* (make-hash-table)))
    ;; Attempt the parse
    (multiple-value-bind (result success newpos) (parseq-internal rule sequence pos)
      ;; Check for success and sequence bounds and return success or failure
      (if (and success (or (and junk-allowed (or (null end) (< (treepos-highest newpos) end))) (= (or end (length sequence)) (treepos-highest newpos))))
          (values result t)
          (if parse-error
              (if (not success)
                  (let ((fail-pos (car *terminal-failure-list*)) (terminals (reverse (cdr *terminal-failure-list*))))
                    (if terminals
                        (f-error parse-match-error (:position fail-pos :terminals terminals)
                                 "Parse error: Expected ~:[~;~:*~{~s~^, ~} or ~]~s at position ~a." (butlast terminals) (last-1 terminals) (treepos-str fail-pos))
                        (f-error parse-match-error (:position fail-pos :terminals terminals)
                                 "Parse error at position ~a." (treepos-str fail-pos))))
                  (f-error parse-junk-error () "Parse error: Junk at the end of the sequence starting at position ~a." (treepos-str newpos)))
              (values nil nil))))))

(defun parseq-internal (rule sequence pos)
  ;; Function that looks up a named rule and calls it when found.
  ;; Is called by `parseq' and the parse rule functions.
  (cond
    ;; Rule is a named rule (with or without args)
    ((or (symbolp rule) (and (consp rule) (symbolp (first rule))))
     ;; Valid rule call. Get the function from the rule table.
     (let ((fun (gethash (if (consp rule) (first rule) rule) *rule-table*)))
       (if fun
           (apply fun sequence pos (if (listp rule) (rest rule)))
           (f-error unknown-rule-error () "Unknown rule: ~a" rule))))
    ;; Invalid rule call
    (t (f-error invalid-rule-error () "Invalid rule: ~a" rule))))

;; Expansion helper macros ---------------------------------------------------

(defun push-terminal-failure (pos terminal)
  (cond
    ((treepos= pos (car *terminal-failure-list*)) (pushnew terminal (cdr *terminal-failure-list*) :test #'equal))
    ((treepos> pos (car *terminal-failure-list*)) (setf (car *terminal-failure-list*) (treepos-copy pos)
                                                        (cdr *terminal-failure-list*) (list terminal))))
  nil)

(defmacro test-and-advance (terminal expr pos test result &optional (inc 1))
  ;; Executes the given test at the given position in the sequence and if
  ;; it succeeds increments the position and returns the sequence item that
  ;; matched.
  (with-gensyms (tmp)
    `(if (treepos-valid ,pos ,expr)
         (if ,test
             (let ((,tmp ,result))
               (setf ,pos (treepos-step ,pos ,inc))
               (values ,tmp t))
             (push-terminal-failure ,pos ,terminal))
         (push-terminal-failure ,pos ,terminal))))

(defmacro try-and-advance (func pos)
  ;; Calls the given function and when successful advances the position
  ;; in the sequence. Success is determined by the second return value
  ;; of func and the amount of advancement by the third return value.
  (with-gensyms (result success newpos)
    `(multiple-value-bind (,result ,success ,newpos) ,func
       (when ,success
         (setf ,pos (treepos-copy ,newpos))
         (values ,result t)))))

(defmacro with-expansion (((result-var success-var) expr rule pos args) &body body)
  ;; Expands the given rule and binds the return values (result and success) to the
  ;; given symbols;
  `(multiple-value-bind (,result-var ,success-var) ,(expand-rule expr rule pos args)
     ,@body))

(defmacro with-expansion-success (((result-var success-var) expr rule pos args) then else)
  ;; Expands the given rule, binds the return values (result and success) to the given
  ;; symbols and evaluates `then' when successful and `else' upon failure.
  `(with-expansion ((,result-var ,success-var) ,expr ,rule ,pos ,args)
     (if ,success-var ,then ,else)))

(defmacro with-expansion-failure (((result-var success-var) expr rule pos args) then else)
  ;; Same as with-expansio-success but with `then' and `else' reversed.
  `(with-expansion-success ((,result-var ,success-var) ,expr ,rule ,pos ,args) ,else ,then))

;; Runtime dispatch ----------------------------------------------------------

(defmacro runtime-match (terminal expr pos test result &optional (inc 1))
  `(if (treepos-valid ,pos ,expr)
       (if ,test
           (values ,result t (treepos-step ,pos ,inc))
           (push-terminal-failure ,pos ,terminal))
       (push-terminal-failure ,pos ,terminal)))

(defun runtime-dispatch (expr arg pos)
  ;; Function that parses terminals at runtime. This is used
  ;; when the type of terminal is unknown at compile time
  ;; (such as for rule arguments).

  ;; Expand terminals
  (loop for expander in *terminal-table* do
    (destructuring-bind (symb test expand runtime) expander
      (declare (ignore symb expand))
      (when (and runtime (funcall test arg))
        (return-from runtime-dispatch (funcall runtime expr arg pos)))))

  ;; Expand nonterminals
  (cond
    ;; Is a symbol (possibly a valid nonterminal)
    ((symbolp arg) (parseq-internal arg expr pos))
    ;; Is a list (possibly a valid nonterminal with arguments)
    ((listp arg) (parseq-internal arg expr pos))
    ;; Not implemented
    (t (f-error invalid-terminal-runtime-error () "Unknown operation: ~a (of type ~a)" arg (type-of arg)))))

;; Operators ---------------------------------------------------------

(defmacro with-backtracking ((pos) &body body)
  (with-gensyms (oldpos)
    `(let ((,oldpos (treepos-copy ,pos)))
       (multiple-value-bind (result success) (progn ,@body)
         (unless success
           (setf ,pos ,oldpos))
         (values result success)))))

(defmacro define-operator (name (expr-var rule-var pos-var args-var) match-code &body expander-code)
  (with-gensyms (index expandfunc)
    `(let ((,index (position ',name *operator-table* :key #'first))
           (,expandfunc (lambda (,expr-var ,rule-var ,pos-var ,args-var) (unless ,match-code (f-error invalid-operation-error () "Invalid (~a ...) expression." ',name)) ,@expander-code)))
       (if ,index
           (setf (elt *operator-table* ,index) (list ',name ,expandfunc))
           (setf *operator-table* (append *operator-table* (list (list ',name ,expandfunc)))))
       *operator-table*)))

(define-operator or (expr rule pos args) (l> rule 0)
  (if (l= rule 1)
      ;; Reduce overhead if there is only one alternative
      (expand-rule expr (first rule) pos args)
      ;; Normal (OR ...) call with multiple alternatives
      `(or2 ,@(loop for r in rule collect (expand-rule expr r pos args)))))

(define-operator and (expr rule pos args) (l> rule 0)
  (with-gensyms (list result block success)
    ;; Block to return from when short-circuiting
    `(with-backtracking (,pos)
       ;; Initialize the list of results
       (block ,block
         (let (,list)
           ;; Loop over the rules
           ,@(loop for r in rule for n upfrom 0 collect
                   ;; Bind a variable to the result of the rule expansion
                   `(with-expansion-success ((,result ,success) ,expr ,r ,pos ,args)
                      ;; Success
                      (appendf ,list ,result)
                      ;; Failure: short-circuit
                      (return-from ,block)))
           ;; Return success
           (values ,list t))))))

(define-operator and~ (expr rule pos args) (l> rule 0)
  ;; Generates code that parses an expression using (and~ ...)
  (with-gensyms (results checklist result success index)
    ;; Make a check list that stores nil for rules that have not yet been applied and t for those that have
    ;; Also make a list of results. We need both lists, because the result of a rule may be nil, even if it succeeds.
    `(with-backtracking (,pos)
       (let ((,checklist (make-list ,(list-length rule) :initial-element nil))
             (,results (make-list ,(list-length rule) :initial-element nil)))
         ;; Check each remaining rule whether it matches the next sequence item
         (loop repeat ,(list-length rule) do
           ;; Try each rule, except those that have already succeeded
           (multiple-value-bind (,result ,success ,index) (or2-exclusive (,checklist) ,@(loop for r in rule collect (expand-rule expr r pos args)))
             ;; If none of the sub-rules succeeded, the rule fails entirely
             (unless ,success
               (return))
             ;; Check the succeeded rule in the list
             (setf (nth ,index ,checklist) t)
             ;; Add the result to the list of results
             (setf (nth ,index ,results) ,result)))
         (unless (some #'null ,checklist)
           ;; Return list of results
           (values ,results t))))))

(defun make-checklist (counts ranges)
  (mapcar (lambda (count range) (and (second range) (>= count (second range)))) counts ranges))

(define-operator and~~ (expr rule pos args) (l> rule 1)
  (let ((rep (first rule)) (rule (rest rule)))
    ;; Generates code that parses an expression using (and~~ ...)
    (with-gensyms (results counts ranges result success index)
      ;; Make a check list that stores the number of times a rule has been applied.
      ;; Also make a list of results and one that stores the range of allowed rule applications.
      `(with-backtracking (,pos)
         (let ((,counts (make-list ,(list-length rule) :initial-element 0))
               (,results (make-list ,(list-length rule) :initial-element nil))
               (,ranges (list ,@(loop for r in rep collect `(list ,@(decode-range r))))))
           ;; Check each rule whether it matches the next sequence item
           (loop do
             ;; Try each rule, except those that have already exceeded their maximum allowed applications
             (multiple-value-bind (,result ,success ,index) (or2-exclusive ((make-checklist ,counts ,ranges)) ,@(loop for r in rule collect (expand-rule expr r pos args)))
               ;; If none of the sub-rules succeeded, the rule fails entirely
               (unless ,success
                 (return))
               ;; Check the succeeded rule in the list
               (incf (nth ,index ,counts))
               ;; Add the result to the list of results
               (appendf (nth ,index ,results) ,result)))
           ;; Catch loop failure
           (unless (some #'null (mapcar #'check-range ,counts ,ranges))
             (values ,results t)))))))

(define-operator not (expr rule pos args) (l>= rule 1)
  ;; Generates code that parses an expression using (not ...)
  (with-gensyms (result success)
    ;; Save the current position
    `(with-backtracking (,pos)
       (with-expansion-failure ((,result ,success) ,expr (or ,@rule) ,pos ,args)
         ;; Expression failed, which is good (but only if we have not reached the end of expr)
         (when (treepos-valid ,pos ,expr)
             (let ((,result (treeitem ,pos ,expr)))
               ;; Advance the position by one
               (setf ,pos (treepos-step ,pos))
               (values ,result t)))
         ;; Expression succeeded, which is bad
         (progn
           ;; Use the variable in order to avoid causing a warning
           ,result
           ;; Return nil
           (values nil nil))))))

(define-operator * (expr rule pos args) (l>= rule 1)
  ;; Generates code that parses an expression using (* ...)
   (with-gensyms (ret)
     `(values (loop for ,ret = (multiple-value-list ,(expand-rule expr `(or ,@rule) pos args)) while (second ,ret) collect (first ,ret)) t)))

(define-operator + (expr rule pos args) (l>= rule 1)
  ;; Generates code that parses an expression using (+ ...)
  (with-gensyms (result success ret)
    `(with-expansion-success ((,result ,success) ,expr (or ,@rule) ,pos ,args)
       (values (append (list ,result) (loop for ,ret = (multiple-value-list ,(expand-rule expr `(or ,@rule) pos args)) while (second ,ret) collect (first ,ret))) t)
       (values nil nil))))

(define-operator rep (expr rule pos args) (l>= rule 2)
  ;; Generates code that parses an expression using (rep ...)
  (destructuring-bind (min max) (decode-range (first rule))
    (with-gensyms (ret results n)
      `(let ((,results (loop
                          for ,n upfrom 0
                          for ,ret = (when (or (null ,max) (< ,n ,max)) (multiple-value-list ,(expand-rule expr `(or ,@(rest rule)) pos args)))
                          while (second ,ret)
                          collect (first ,ret))))
         (if (and (or (null ,min) (l>= ,results ,min)) (or (null ,max) (l<= ,results ,max)))
             (values ,results t)
             (values nil nil))))))

(define-operator ? (expr rule pos args) (l>= rule 1)
  ;; Generates code that parses an expression using (? ...)
  (with-gensyms (result success)
    `(with-expansion ((,result ,success) ,expr (or ,@rule) ,pos ,args)
       (values (if ,success ,result nil) t))))

(define-operator & (expr rule pos args) (l= rule 1)
  ;; Generates code that parses an expression using (& ...)
  (with-gensyms (oldpos result success)
    `(let ((,oldpos (treepos-copy ,pos)))
       (with-expansion-success ((,result ,success) ,expr ,(first rule) ,pos ,args)
         (progn
           (setf ,pos ,oldpos)
           (values ,result t))
         (values nil nil)))))

(define-operator ! (expr rule pos args) (l= rule 1)
  ;; Generates code that parses an expression using (! ...)
  (with-gensyms (oldpos result success)
    `(let ((,oldpos (treepos-copy ,pos)))
       (with-expansion-failure ((,result ,success) ,expr ,(first rule) ,pos ,args)
         ;; Failure, which is good (but only if we're not at the end of expr)
         (if (treepos-valid ,pos ,expr)
             (let ((,result (treeitem ,pos ,expr)))
               (values ,result t))
             (values nil nil))
         ;; Success, which is bad
         (progn
           (setf ,pos ,oldpos)
           (values ,result nil))))))

(defun expand-sequence (expr rules pos args type-test terminal)
  ;; Generates code that parses an expression using (list ...), (vector ...) or (string ...)
  (with-gensyms (result success length newpos)
    `(when (treepos-valid ,pos ,expr)
       (if (funcall #',type-test (treeitem ,pos ,expr))
           (let ((,length (treepos-length ,pos ,expr)) (,newpos (treepos-step-down ,pos)))
             (with-expansion-success ((,result ,success) ,expr (and ,@rules) ,newpos ,args)
               ;; Success
               (when (= (treepos-lowest ,newpos) ,length)
                 ;; Step out of the list and increment the position
                 (setf ,pos (treepos-step (treepos-copy ,newpos -1)))
                 (values ,result t))
               ;; Failure
               (values nil nil)))
           (push-terminal-failure ,pos ',terminal)))))

(define-operator list (expr rules pos args) (l> rules 0)
  (expand-sequence expr rules pos args 'listp 'list))

(define-operator string (expr rules pos args) (l> rules 0)
  (expand-sequence expr rules pos args 'stringp 'string))

(define-operator vector (expr rules pos args) (l> rules 0)
  (expand-sequence expr rules pos args 'vectorp 'vector))

;; Terminals ---------------------------------------------------------

(defvar *ascii-standard-chars* (list #\Newline #\  #\! #\" #\# #\$ #\% #\& #\' #\( #\) #\* #\+ #\, #\- #\. #\/
                                     #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\: #\; #\< #\= #\> #\? #\@ #\A #\B
                                     #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U
                                     #\V #\W #\X #\Y #\Z #\[ #\\ #\] #\^ #\_ #\` #\a #\b #\c #\d #\e #\f #\g #\h
                                     #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\{
                                     #\| #\} #\~))

(defun expand-regexp-bracket-expression (expr)
  (concatenate 'string
               (delete-duplicates
                (loop for i upfrom 0 below (length expr) for last = ch for ch = (elt expr i)
                   append
                     (cond
                       ((and (char= ch #\-) (zerop i)) (list #\-))
                       ((and (char= ch #\-) (= i (1- (length expr)))) (list #\-))
                       ((char= ch #\-)
                        (let* ((next (elt expr (1+ i)))
                               (a (position last *ascii-standard-chars*))
                               (b (position next *ascii-standard-chars*)))
                          (when (< b a)
                            (f-error invalid-terminal-error () "Invalid character range: \"~c-~c\"" last next))
                          (incf i 1)
                          (subseq *ascii-standard-chars* (1+ a) (1+ b))))
                       (t (list ch))))
                :test #'char= :from-end t)))

;; Macro that facilitates the addition of new terminals
(defmacro define-terminal (name (expr rule pos args) test expander-code runtime-code)
  (with-gensyms (index matchfunc expandfunc runtimefunc)
    `(let ((,index (position ',name *terminal-table* :key #'first))
           (,matchfunc (lambda (,rule) (declare (ignorable ,rule)) ,test))
           (,expandfunc (lambda (,expr ,rule ,pos ,args) (declare (ignorable ,expr ,rule ,pos ,args)) ,expander-code))
           (,runtimefunc (lambda (,expr ,rule ,pos) (declare (ignorable ,expr ,rule ,pos)) ,runtime-code)))
       (if ,index
           (setf (nth ,index *terminal-table*) (list ',name ,matchfunc ,expandfunc ,runtimefunc))
           (setf *terminal-table* (append *terminal-table* (list (list ',name ,matchfunc ,expandfunc ,runtimefunc)))))
       *terminal-table*)))

;; Macro that facilitates the addition of simple terminals
(defmacro define-simple-terminal (name (rule-var item-var &key quote) rule-test item-test)
  (let ((qfun (if (null quote) 'identity 'quote)))
    `(define-terminal ,name (expr ,rule-var pos args)
        ,rule-test
        `(test-and-advance (,',qfun ,,rule-var) ,expr ,pos ((lambda (,',rule-var ,',item-var) (declare (ignorable ,',rule-var ,',item-var)) ,',item-test) (,',qfun ,,rule-var) (treeitem ,pos ,expr)) (treeitem ,pos ,expr))
        (symbol-macrolet ((,item-var (treeitem pos expr)))
          (runtime-match ,rule-var expr pos ,item-test ,item-var)))))

(defmacro define-simple-symbol-terminal (name (rule-var item-var) item-test)
  (assert (symbolp name))
  `(define-simple-terminal ,name (,rule-var ,item-var :quote t) (symbol= ,rule-var ',name) ,item-test))

;; Macro that facilitates the addition of simple sequence terminals
(defmacro define-simple-sequence-terminal (name (seq-test seq-eql))
  (with-gensyms (rule-var)
    `(define-terminal ,name (expr ,rule-var pos args)
      (,seq-test ,rule-var)
      `(test-and-advance ,,rule-var ,expr ,pos (if (,',seq-test (treeitem (treepos-copy ,pos -1) ,expr))
                                                   (subseq-at ,,rule-var (treeitem (treepos-copy ,pos -1) ,expr) (treepos-lowest ,pos))
                                                   (and (,',seq-test (treeitem ,pos ,expr)) (,',seq-eql (treeitem ,pos ,expr) ,,rule-var)))
           ,,rule-var (if (,',seq-test (treeitem (treepos-copy ,pos -1) ,expr)) ,(length ,rule-var) 1))
      (if (,seq-test (treeitem (treepos-copy pos -1) expr))
          (runtime-match ,rule-var expr pos (subseq-at ,rule-var (treeitem (treepos-copy pos -1) expr) (treepos-lowest pos)) ,rule-var (length ,rule-var))
          (runtime-match ,rule-var expr pos (and (,seq-test (treeitem pos expr)) (,seq-eql (treeitem pos expr) ,rule-var)) (treeitem pos expr))))))

;; Function to remove terminal definition
(defun undefine-terminal (name)
  (setf *terminal-table* (remove name *terminal-table* :key #'first))
  *terminal-table*)

;; Tests whether a specific terminal represented by a symbol exists
(defun have-symbol-terminal (symbol)
  (some (lambda (x) (symbol= (first x) symbol)) *terminal-table*))

;; Define terminals
(define-simple-terminal (specific-character) (rule item) (characterp rule) (and (characterp item) (char= item rule)))
(define-simple-terminal (specific-number) (rule item) (numberp rule) (and (numberp item) (= item rule)))
(define-simple-terminal (specific-symbol) (rule item :quote t) (quoted-symbol-p rule) (symbol= item (second rule)))
(define-simple-terminal (character-set) (rule item :quote t) (and (listp rule) (l= rule 2) (symbol= (first rule) 'char) (stringp (second rule))) (and (characterp item) (find item (expand-regexp-bracket-expression (second rule)))))
(define-simple-sequence-terminal (specific-string) (stringp string=))
(define-simple-sequence-terminal (specific-vector) (vectorp equalp))
(define-simple-symbol-terminal t (rule item) (not (null item)))
(define-simple-symbol-terminal nil (rule item) (null item))
(define-simple-symbol-terminal char (rule item) (characterp item))
(define-simple-symbol-terminal stdchar (rule item) (and (characterp item) (standard-char-p item)))
(define-simple-symbol-terminal alpha (rule item) (and (characterp item) (alpha-char-p item)))
(define-simple-symbol-terminal digit (rule item) (and (characterp item) (digit-char-p item)))
(define-simple-symbol-terminal alphanumeric (rule item) (and (characterp item) (standard-char-p item) (alphanumericp item)))
(define-simple-symbol-terminal byte (rule item) (unsigned-byte-p item))
(define-simple-symbol-terminal symbol (rule item) (symbolp item))
(define-simple-symbol-terminal keyword (rule item) (keywordp item))
(define-simple-symbol-terminal form (rule item) t)
(define-simple-symbol-terminal atom (rule item) (atom item))
(define-simple-symbol-terminal list (rule item) (listp item))
(define-simple-symbol-terminal cons (rule item) (consp item))
(define-simple-symbol-terminal vector (rule item) (vectorp item))
(define-simple-symbol-terminal number (rule item) (numberp item))
(define-simple-symbol-terminal integer (rule item) (integerp item))
(define-simple-symbol-terminal string (rule item) (stringp item))

;; Rule expansion ----------------------------------------------------

(defun expand-parse-call-recursion (rule args)
  (loop for r in rule for n upfrom 0 collect
       (cond
         ((and (plusp n) (have r args)) r)
         ((and (plusp n) (listp r)) `(list ,@(expand-parse-call-recursion r args)))
         (t `(quote ,r)))))

(defun expand-parse-call (expr rule pos args)
  ;; Makes a call to `parseq-internal' with or without quoting the rule arguments depending on whether they are arguments to the current rule
  `(parseq-internal (list ,@(expand-parse-call-recursion rule args)) ,expr ,pos))

(defun expand-rule (expr rule pos args)
  ;; Generates code according to the given rule
  ;; Expand terminals
  (loop for expander in *terminal-table* do
    (destructuring-bind (symb test expand runtime) expander
      (declare (ignore symb runtime))
      (when (funcall test rule)
          (return-from expand-rule (funcall expand expr rule pos args)))))
  ;; Expand operators
  (loop for op in *operator-table* do
    (destructuring-bind (symb expandfunc) op
      (when (and (consp rule) (symbol= (first rule) symb))
        (return-from expand-rule (funcall expandfunc expr (rest rule) pos args)))))
  ;; Expand nonterminals
  (cond
    ;; Expand nonterminals (without args)
    ((and (symbolp rule) (have rule args)) `(try-and-advance (runtime-dispatch ,expr ,rule ,pos) ,pos))
    ((symbolp rule) `(try-and-advance (parseq-internal ',rule ,expr ,pos) ,pos))
    ;; Expand nonterminal (with args)
    ((and (consp rule) (symbolp (first rule))) `(try-and-advance ,(expand-parse-call expr rule pos args) ,pos))
    ;; Invalid operation
    (t (f-error invalid-operation-error () "Invalid operation ~s" rule))))

;; Rule options --------------------------------------------------------------

(defun expand-destructure (destruct-lambda result body)
  ;; Generates code for handling a (:destructure ...) or (:lambda ...) rule option
  `(destructuring-bind ,destruct-lambda (mklist ,result) ,@body))

(defun expand-choice (indices result)
  ;; Generates code for handling a (:choose ...) rule option.
  (with-gensyms (var list)
    (if (l= indices 1)
        ;; Return the item
        `(choice-item ,(first indices) (mklist ,result))
        ;; Return a list of items
        `(loop for ,var in (list ,@indices) with ,list = (mklist ,result) collect (choice-item ,var ,list)))))

(defun expand-processing-options (result procs)
  ;; Generates code for handling rule options
  (with-gensyms (blockname tmp)
    (if (null procs)
        `(values ,result t)
        `(block ,blockname
           ;; Save the result in a temporary variable
           (let ((,tmp ,result))
             ;; Execute the procs in order
             ,@(loop for opt in procs collect
                    (cond
                      ((and (l= opt 2) (eql (first opt) :constant)) `(setf ,tmp ,(second opt)))
                      ((and (l> opt 1) (eql (first opt) :lambda) (listp (second opt))) `(setf ,tmp ,(expand-destructure (second opt) tmp (cddr opt))))
                      ((and (l> opt 1) (eql (first opt) :destructure) (listp (second opt))) `(setf ,tmp ,(expand-destructure (second opt) tmp (cddr opt))))
                      ((and (l> opt 1) (eql (first opt) :choose)) `(setf ,tmp ,(expand-choice (rest opt) tmp)))
                      ((and (l= opt 2) (eql (first opt) :function)) `(setf ,tmp (apply ,(second opt) (mklist ,tmp))))
                      ((and (l= opt 2) (eql (first opt) :identity)) `(unless ,(second opt) (setf ,tmp nil)))
                      ((and (l= opt 1) (eql (first opt) :flatten)) `(setf ,tmp (if (listp ,tmp) (flatten ,tmp) (list ,tmp))))
                      ((and (l= opt 1) (eql (first opt) :string)) `(setf ,tmp (apply #'cat (if (listp ,tmp) (flatten ,tmp) (list ,tmp)))))
                      ((and (l= opt 1) (eql (first opt) :vector)) `(setf ,tmp (apply #'vector (if (listp ,tmp) (flatten ,tmp) (list ,tmp)))))
                      ((and (l> opt 1) (eql (first opt) :test) (listp (second opt))) `(unless ,(expand-destructure (second opt) tmp (cddr opt)) (return-from ,blockname)))
                      ((and (l> opt 1) (eql (first opt) :not) (listp (second opt))) `(when ,(expand-destructure (second opt) tmp (cddr opt)) (return-from ,blockname)))
                      (t (f-error processing-options-error () "Invalid processing option ~s." opt))))
             (values ,tmp t))))))

(defmacro rule-options-bind ((specials externals processing packrat) options name &body body)
  (with-gensyms (opt pckrt)
    `(multiple-value-bind (,specials ,externals ,processing ,packrat)
         (loop for ,opt in ,options
            with ,pckrt = nil
            when (not (consp ,opt)) do (f-error processing-options-error () "Invalid processing option in rule definition for ~a." ,name)
            when (eql (first ,opt) :external) append (rest ,opt) into ,externals
            when (eql (first ,opt) :let) append (rest ,opt) into ,specials
            when (and (l= ,opt 2) (eql (first ,opt) :packrat)) do (setf ,pckrt (not (not (second ,opt))))
            when (have (first ,opt) '(:constant :lambda :destructure :choose :function :identity :flatten :string :vector :test :not))
            collect ,opt into ,processing
            when (not (have (first ,opt) '(:constant :lambda :destructure :function :choose :identity :flatten :string :vector :test :not :external :let :packrat)))
            do (f-error processing-options-error () "Invalid processing option ~s in rule definition for ~a." (first ,opt) ,name)
            finally (return (values ,specials ,externals ,processing ,pckrt)))
       ,@body)))

;; Special variables (rule bindings) -----------------------------------------

(defmacro with-special-vars-from-options (bindings &body body)
  ;; Generates code that declares the special variables given in the rule options
  (if bindings
      `(let-special (,@bindings) ,@body)
      `(progn ,@body)))

;; Trace functions -----------------------------------------------------------

;; Counter that is incremented whenever a traced rule is called and decremented when it returns
(defparameter *trace-depth* 0)
;; Flag that can be shadowed by a traced rule so that rules called from within that rule can be traced as well
(defparameter *trace-recursive* nil)
;; Lookup table for rules that specifies whether a rule is traced (value 1) or traced recursively (value 2) or not (value 0).
(defparameter *trace-rule* (make-hash-table :test 'equal))

(defun is-traced (trace-option)
  ;; Determies, whether a rule is traced or not depending on the given option and the *trace-recursive* parameter.
  (or (plusp trace-option) *trace-recursive*))

(defun is-traced-with-recursion (trace-option)
  (= trace-option 2))

(defmacro with-tracing ((name pos memo) &body body)
  ;; Wrapper for enabling tracing in parse rules
  (with-gensyms (trace-opt result success newpos)
    ;; Lookup tracing options in the hash table.
    ;; This actually closes over the symbol `name' so the parsing function remembers which name it was defined with.
    `(let ((,trace-opt (gethash (symbol-name ',name) *trace-rule*)) ,memo)
       (conditional-dynamic-bind ((*trace-recursive* (if (is-traced-with-recursion ,trace-opt) t *trace-recursive*))
                                  (*trace-depth* (1+ *trace-depth*)))
           (is-traced ,trace-opt)
         ;; Print trace start
         (when (is-traced ,trace-opt)
           (format t "~v,0T~d: ~a ~a?~%" (1- *trace-depth*) *trace-depth* ',name (treepos-str ,pos)))
         ;; Run the code and intercept the return values
         (multiple-value-bind (,result ,success ,newpos) (progn ,@body)
           ;; Print the end of the trace
           (when (is-traced ,trace-opt)
             ;; Different format depending on success
             (if ,success
                 (format t "~v,0T~d: ~a ~a-~a -> ~s~:[~; (memoized)~]~%" (1- *trace-depth*) *trace-depth* ',name (treepos-str ,pos) (treepos-str ,newpos) ,result ,memo)
                 (format t "~v,0T~d: ~a -|~:[~; (memoized)~]~%" (1- *trace-depth*) *trace-depth* ',name ,memo)))
           ;; Return interceptet return values
           (values ,result ,success ,newpos))))))

(defun trace-rule (name &key recursive)
  ;; Function that enables tracing of the given rule
  (setf (gethash (symbol-name name) *trace-rule*) (if recursive 2 1)))

(defun untrace-rule (name)
  ;; Function that disables tracing of the given rule
  (setf (gethash (symbol-name name) *trace-rule*) 0))

;; Left recursion -------------------------------------------------------------

(defmacro with-left-recursion-protection ((pos stack) &body body)
  ;; Wrapper for rule definitions that checks for left recursion
  ;; by keeping track of whether the rule has been called at the
  ;; given position in the parsed sequence or not. The stack is
  ;; a closed over variable where the current position is pushed
  ;; on when a rule is called and popped when it returns.
  `(progn
     (when (and ,stack (treepos= ,pos (first ,stack)))
       (f-error left-recursion-error () "Left recursion detected!"))
     ;; Save the position in which this rule was called
     (push (treepos-copy ,pos) ,stack)
     ;; Execute the body.
     (unwind-protect (progn ,@body)
       ;; Make sure the position is popped from the stack /always/.
       (pop ,stack))))

;; Packrat --------------------------------------------------------------------

(defun make-packrat-key (treepos lambda-list external-bindings)
  "Generates a key for the packrat memoization hash table."
  (if (or lambda-list external-bindings)
      `(list ,treepos ,@(when lambda-list (list `(list ,@(lambda-list-vars lambda-list)))) ,@(when external-bindings (list `(list ,@external-bindings))))
      treepos))

(defmacro with-packrat ((enabled name pos lambda-list external-bindings memo) &body body)
  (with-gensyms (blockname memo-table values)
    (if enabled
        `(block ,blockname
           ;; Are any values already stored for the current function?
           (if-hash (',name *packrat-table* :var ,memo-table :place t)
                    ;; Values already stored. Check whether the current function call is memoized.
                    (if-hash (,(make-packrat-key pos lambda-list external-bindings) ,memo-table :var ,values)
                             (progn
                               (setf ,memo t)
                               (return-from ,blockname (apply #'values ,values))))
                    ;; No values stored, create hash table
                    (setf ,memo-table (make-hash-table :test 'equalp)))
           ;; Run the body, results in a list
           (let ((,values (multiple-value-list (progn ,@body))))
             ;; Store the results in the packrat table, if necessary
             (setf (gethash ,(make-packrat-key pos lambda-list external-bindings) (gethash ',name *packrat-table*)) ,values)
             ;; Return the results again as multiple values
             (apply #'values ,values)))
        `(progn ,@body))))

;; defrule macro --------------------------------------------------------------

(defmacro defrule (name lambda-list expr &body options)
  ;; Creates a lambda expression that parses the given grammar rules.
  ;; It then stores the lambda function in the global list *rule-table*,
  ;; therefore the rule functions use a namespace separate from everything
  (with-gensyms (sequence pos oldpos result success last-call-pos memo)
    ;; Split options into specials, externals and processing options
    (rule-options-bind (specials externals processing-options packrat) options name
      ;; Bind a variable for the following lambda expression to close over
      `(let (,last-call-pos)
         ;; Save the name in the trace rule table (unless it is already there)
         (if-hash ((symbol-name ',name) *trace-rule* :var value :place t) t (setf value 0))
         ;; Save the lambda function in the namespace table
         (setf (gethash ',name *rule-table*)
               ;; Lambda expression that parses according to the given grammar rule
               (lambda (,sequence ,pos ,@lambda-list)
                 ;; Declare special variables specified in the (:external ...) option
                 ,@(if externals `((declare (special ,@externals))))
                 ;; Check for left recursion
                 (with-left-recursion-protection (,pos ,last-call-pos)
                   ;; Bind special variables from the (:let ...) option
                   (with-special-vars-from-options ,specials
                     ;; Save the previous parsing position
                     (let ((,pos (treepos-copy ,pos)) (,oldpos (treepos-copy ,pos)))
                       ;; Print tracing information
                       (with-tracing (,name ,oldpos ,memo)
                         ;; Memoization (packrat parsing)
                         (with-packrat (,packrat ,name ,oldpos ,lambda-list ,externals ,memo)
                           ;; Expand the rule into code that parses the sequence
                           (with-expansion-success ((,result ,success) ,sequence ,expr ,pos ,lambda-list)
                             ;; Process the result
                             (multiple-value-bind (,result ,success) ,(expand-processing-options result processing-options)
                               ;; Processing of (:test ...) and (:not ...) options may make the parse fail
                               (if ,success
                                   ;; Return the processed parsing result, the success and the new position
                                   (values ,result t ,pos)
                                   ;; Processing causes parse to fail
                                   (values nil nil ,oldpos)))
                             ;; Return nil as parsing result, failure and the old position
                             (values nil nil ,oldpos)))))))))))))

;; Namespace macros and functions ---------------------------------------------

(defmacro with-local-rules (&body body)
  ;; Shadow the global rule table with a new rule table
  `(let ((*rule-table* (make-hash-table))
         (*trace-rule* (make-hash-table)))
     ;; Execute the body
     ,@body))

(defmacro with-saved-rules (&body body)
  ;; Shadow the global rule table with a copy of the rule table
  ;; When returninng from the body the original rules are restored
  `(let ((*rule-table* (copy-hash-table *rule-table*))
         (*trace-rule* (copy-hash-table *trace-rule*)))
     ;; Execute the body
     ,@body))

(defun clear-rules ()
  (clrhash *rule-table*))
