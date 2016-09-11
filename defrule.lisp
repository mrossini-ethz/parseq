(in-package :parseq)

;; Hash table that maps the rule names (symbols) to the functions
;; that parse the corresponding rule. Being a special variable,
;; it can be shadowed. This is used in the macro with-local-rules.
(defparameter *rule-table* (make-hash-table))

(defun parseq (rule sequence &key (start 0) end junk-allowed)
  ;; Parses sequence according to the given rule. A subsequence
  ;; can be parsed by passing the start and/or end arguments.
  ;; The parse does fails if the end of the sequence is not reached
  ;; unless junk-allowed is given.
  (let ((pos (list start)))
    ;; Attempt the parse
    (multiple-value-bind (result success newpos) (parseq-internal rule sequence pos)
      ;; Check for success and sequence bounds and return success or failure
      (if (and success (or (and junk-allowed (or (null end) (< (first newpos) end))) (= (or end (length sequence)) (first newpos))))
          (values result t)
          (values nil nil)))))

(defun parseq-internal (rule sequence pos)
  ;; Function that looks up a named rule and calls it when found.
  ;; Is called by `parseq' and the parse rule functions.
  (cond
    ;; Rule is a named rule (with or without args)
    ((or (symbolp rule) (and (listp rule) (symbolp (first rule))))
     ;; Get the function from the rule table
     (let ((fun (gethash (if (listp rule) (first rule) rule) *rule-table*)))
       (if fun
           (apply fun sequence pos (if (listp rule) (rest rule)))
           (error "Unknown rule: ~a" rule))))
    (t (error "Invalid rule: ~a" rule))))

;; Expansion helper macros ---------------------------------------------------

(defmacro test-and-advance (expr pos test result &optional (inc 1))
  ;; Executes the given test at the given position in the sequence and if
  ;; it succeeds increments the position and returns the sequence item that
  ;; matched.
  (with-gensyms (tmp)
    `(when (treepos-valid ,pos ,expr)
       (when ,test
         (let ((,tmp ,result))
           (setf ,pos (treepos-step ,pos ,inc))
           (values ,tmp t))))))

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

(defun runtime-dispatch (expr arg pos)
  ;; Function that parses terminals at runtime. This is used
  ;; when the type of terminal is unknown at compile time
  ;; (such as for rule arguments).
  (cond
    ;; Is a quoted symbol
    ((quoted-symbol-p arg) (if (symbol= (second arg) (treeitem pos expr)) (values (second arg) t (treepos-step pos))))
    ;; Is a character
    ((characterp arg) (if (char= arg (treeitem pos expr)) (values arg t (treepos-step pos))))
    ;; Is a string and expression is also a string
    ((and (stringp expr) (stringp arg)) (if (subseq-at arg (treeitem (butlast pos) expr) (last-1 pos)) (values arg t (treepos-step pos (length arg)))))
    ;; Is a string, expression is not a string
    ((stringp arg) (if (string= arg (treeitem pos expr)) (values arg t (treepos-step pos 1))))
    ;; Is a vector and expression is also a vector
    ((and (vectorp expr) (vectorp arg)) (if (subseq-at arg (treeitem (butlast pos) expr) (last-1 pos)) (values arg t (treepos-step pos (length arg)))))
    ;; Is a vector, expression is not a vector
    ((vectorp arg) (if (equalp arg (treeitem pos expr)) (values arg t (treepos-step pos 1))))
    ;; Is a number
    ((numberp arg) (if (= arg (treeitem pos expr)) (values arg t (treepos-step pos))))
    ;; Not implemented
    (t (error "Unknown terminal: ~a (~a)" arg (type-of arg)))))

;; Expansion functions -----------------------------------------------

;; These are helper functions for the defrule macro.
;; Therefore, the functions contain macro code and need to be treated as such.
;; All take the list that should be parsed as `expr', the parsing `rule',
;; the current `pos'ition in the list as well as the `arg'uments to the defrule.
;; The intent is to generate lisp code for parsing.
;; They return two values: The portion of the `expr' that was parsed, and a success value

(defun expand-atom (expr rule pos args)
  ;; Generates code that parses a lisp atom.
  (cond
    ;; Is a quoted symbol
    ((quoted-symbol-p rule) `(test-and-advance ,expr ,pos (symbol= (treeitem ,pos ,expr) ,rule) (treeitem ,pos ,expr)))
    ;; Is a character
    ((characterp rule) `(test-and-advance ,expr ,pos (char= (treeitem ,pos ,expr) ,rule) (treeitem ,pos ,expr)))
    ;; Is a string
    ((stringp rule) `(test-and-advance ,expr ,pos (if (stringp (treeitem (butlast ,pos) ,expr))
                                                      ;; We are parsing a string, so match substring
                                                      (subseq-at ,rule (treeitem (butlast ,pos) ,expr) (last-1 ,pos))
                                                      ;; We are not parsing a string, match the whole item
                                                      (string= (treeitem ,pos ,expr) ,rule))
                                       ,rule (if (stringp (treeitem (butlast ,pos) ,expr)) ,(length rule) 1)))
    ;; Is a vector
    ((vectorp rule) `(test-and-advance ,expr ,pos (if (vectorp (treeitem (butlast ,pos) ,expr))
                                                      ;; We are parsing a vector, match the subsequence
                                                      (subseq-at ,rule (treeitem (butlast ,pos) ,expr) (last-1 ,pos))
                                                      ;; We are not parsing a vector, match the whole item
                                                      (equalp (treeitem ,pos ,expr) ,rule))
                                       ,rule (if (vectorp (treeitem (butlast ,pos) ,expr)) ,(length rule) 1)))
    ;; Is a number
    ((numberp rule) `(test-and-advance ,expr ,pos (= ,rule (treeitem ,pos ,expr)) ,rule))
    ;; Is a symbol
    ((symbolp rule)
     (cond
       ;; Is a lambda variable. Since we don't know what the value is at compile time, we have to dispatch at runtime
       ((have rule args) `(try-and-advance (runtime-dispatch ,expr ,rule ,pos) ,pos))
       ;; Is the symbol 't'
       ((eql rule t) `(test-and-advance ,expr ,pos (not (null (treeitem ,pos ,expr))) (treeitem ,pos ,expr)))
       ;; Is the symbol 'nil'
       ((null rule) `(test-and-advance ,expr ,pos (null (treeitem ,pos ,expr)) nil))
       ;; Is the symbol 'char'
       ((symbol= rule 'char) `(test-and-advance ,expr ,pos (characterp (treeitem ,pos ,expr)) (treeitem ,pos, expr)))
       ;; Is the symbol 'byte'
       ((symbol= rule 'byte) `(test-and-advance ,expr ,pos (unsigned-byte-p (treeitem ,pos ,expr)) (treeitem ,pos ,expr)))
       ;; Is the symbol 'symbol'
       ((symbol= rule 'symbol) `(test-and-advance ,expr ,pos (symbolp (treeitem ,pos ,expr)) (treeitem, pos, expr)))
       ;; Is the symbol 'form'
       ((symbol= rule 'form) `(test-and-advance ,expr ,pos t (treeitem ,pos, expr)))
       ;; Is the symbol 'list'
       ((symbol= rule 'list) `(test-and-advance ,expr ,pos (listp (treeitem ,pos ,expr)) (treeitem, pos, expr)))
       ;; Is the symbol 'vector'
       ((symbol= rule 'vector) `(test-and-advance ,expr ,pos (vectorp (treeitem ,pos ,expr)) (treeitem, pos, expr)))
       ;; Is the symbol 'number'
       ((symbol= rule 'number) `(test-and-advance ,expr ,pos (numberp (treeitem ,pos ,expr)) (treeitem, pos, expr)))
       ;; Is the symbol 'string'
       ((symbol= rule 'string) `(test-and-advance ,expr ,pos (stringp (treeitem ,pos ,expr)) (treeitem, pos, expr)))
       ;; Is a call to another rule (without args)
       (t `(try-and-advance (parseq-internal ',rule ,expr ,pos) ,pos))))
    (t (error "Unknown atom: ~a (~a)" rule (type-of rule)))))

(defun expand-or (expr rule pos args)
  ;; Generates code that parses an expression using (or ...)
  `(or2 ,@(loop for r in rule collect (expand-rule expr r pos args))))

(defun expand-and~ (expr rule pos args)
  ;; Generates code that parses an expression using (and~ ...)
  (with-gensyms (results checklist result success index)
    ;; Make a check list that stores nil for rules that have not yet been applied and t for those that have
    ;; Also make a list of results. We need both lists, because the result of a rule may be nil, even if it succeeds.
    `(let ((,checklist (make-list ,(list-length rule) :initial-element nil))
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
       ;; Catch loop failure
       (unless (some #'null ,checklist)
         ;; Return list of results
         (values ,results t)))))

(defun expand-and (expr rule pos args)
  ;; Generates code that parses an expression using (and ...)
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
  ;; Generates cod that parses an expression using (not ...)
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
  ;; Generates cod that parses an expression using (* ...)
   (with-gensyms (ret)
     `(values
       (loop for ,ret = (multiple-value-list ,(expand-rule expr rule pos args)) while (second ,ret) collect (first ,ret))
       t)))

(defun expand-+ (expr rule pos args)
  ;; Generates cod that parses an expression using (+ ...)
  (with-gensyms (result success ret)
    `(with-expansion-success ((,result ,success) ,expr ,rule ,pos ,args)
       (values
        (append (list ,result) (loop for ,ret = (multiple-value-list ,(expand-rule expr rule pos args)) while (second ,ret) collect (first ,ret)))
        t)
       (values nil nil))))

(defun expand-rep (range expr rule pos args)
  ;; Generates cod that parses an expression using (rep ...)
  (let (min max)
    (cond
      ((or (symbolp range) (numberp range)) (setf min range max range))
      ((and (listp range) (l= range 1)) (setf min 0 max (first range)))
      ((and (listp range) (l= range 2)) (setf min (first range) max (second range)))
      (t (error "Illegal range specified!")))
    (with-gensyms (ret results n)
      `(let ((,results (loop for ,n upfrom 0 for ,ret = (when (< ,n ,max) (multiple-value-list ,(expand-rule expr rule pos args))) while (second ,ret) collect (first ,ret))))
         (if (and (l>= ,results ,min) (l<= ,results ,max))
             (values ,results t)
             (values nil nil))))))

(defun expand-? (expr rule pos args)
  ;; Generates cod that parses an expression using (? ...)
  (with-gensyms (result success)
    `(with-expansion ((,result ,success) ,expr ,rule ,pos ,args)
       (values (if ,success ,result nil) t))))

(defun expand-& (expr rule pos args)
  ;; Generates cod that parses an expression using (& ...)
  (with-gensyms (oldpos result success)
    `(let ((,oldpos (treepos-copy ,pos)))
       (with-expansion-success ((,result ,success) ,expr ,rule ,pos ,args)
         (progn
           (setf ,pos ,oldpos)
           (values ,result t))
         (values nil nil)))))

(defun expand-! (expr rule pos args)
  ;; Generates cod that parses an expression using (! ...)
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

(defun expand-sequence (expr rule pos args type-test)
  ;; Generates cod that parses an expression using (list ...), (vector ...) or (string ...)
  (with-gensyms (result success length)
    `(when (and (treepos-valid ,pos ,expr) (funcall #',type-test (treeitem ,pos ,expr)))
       (let ((,length (treepos-length ,pos ,expr)))
         ;; Go into the list
         (appendf ,pos 0)
         (with-expansion-success ((,result ,success) ,expr ,rule ,pos ,args)
           ;; Success
           (when (= (last-1 ,pos) ,length)
             ;; Step out of the list and increment the position
             (setf ,pos (treepos-step (butlast ,pos)))
             (values (list ,result) t))
           ;; Failure
           (values nil nil))))))

(defun expand-parse-call (expr rule pos args)
  ;; Makes a call to `parseq-internal' with or without quoting the rule arguments depending on whether they are arguments to the current rule
  `(parseq-internal `(,,@(loop for r in rule for n upfrom 0 collect (if (and (plusp n) (have r args)) r `(quote ,r)))) ,expr ,pos))

(defun expand-list-expr (expr rule pos args)
  ;; Generates code that parses an expression with a rule that is a list
  ;; Rule is a ...
  (case-test ((first rule) :test symbol=)
    ;; ordered choice
    (or (expand-or expr (rest rule) pos args))
    ;; sequence
    (and (expand-and expr (rest rule) pos args))
    ;; sequence (unordered)
    (and~ (expand-and~ expr (rest rule) pos args))
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
    (list (expand-sequence expr (second rule) pos args 'listp))
    ;; string
    (string (expand-sequence expr (second rule) pos args 'stringp))
    ;; vector
    (vector (expand-sequence expr (second rule) pos args 'vectorp))
    ;; repetition
    (rep (expand-rep (cadr rule) expr (caddr rule) pos args))
    ;; a call to another rule (with args)
    (t `(try-and-advance ,(expand-parse-call expr rule pos args) ,pos))))

(defun expand-rule (expr rule pos args)
  ;; Generates code according to the given rule
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
  ;; Generates code for handling a (:destructure ...) or (:lambda ...) rule option
  `(destructuring-bind ,destruct-lambda (mklist ,result) ,@body))

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
                    (case (first opt)
                      (:constant `(setf ,tmp ,(second opt)))
                      (:lambda `(setf ,tmp ,(expand-destructure (second opt) tmp (cddr opt))))
                      (:destructure `(setf ,tmp ,(expand-destructure (second opt) tmp (cddr opt))))
                      (:function `(setf ,tmp (apply ,(second opt) (mklist ,tmp))))
                      (:identity `(unless ,(second opt) (setf ,tmp nil)))
                      (:flatten `(setf ,tmp (if (listp ,result) (flatten ,result) (list ,tmp))))
                      (:string `(setf ,tmp (apply #'cat (if (listp ,result) (flatten ,result) (list ,tmp)))))
                      (:vector `(setf ,tmp (apply #'vector (if (listp ,result) (flatten ,result) (list ,tmp)))))
                      (:test `(unless ,(expand-destructure (second opt) tmp (cddr opt)) (return-from ,blockname)))
                      (:not `(when ,(expand-destructure (second opt) tmp (cddr opt)) (return-from ,blockname)))))
             (values ,tmp t))))))

;; Special variables (rule bindings) -----------------------------------------

(defmacro with-special-vars ((&rest vars) &body body)
  ;; Declares the given vars as special. Vars is like the list in the first argument of a let form.
  `(let (,@vars)
     (declare (special ,@(loop for v in vars collect (if (listp v) (first v) v))))
     ,@body))

(defmacro with-special-vars-from-options (bindings &body body)
  ;; Generates code that declares the special variables given in the rule options
  (if bindings
      `(with-special-vars (,@bindings) ,@body)
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

(defmacro with-tracing ((name pos) &body body)
  ;; Wrapper for enabling tracing in parse rules
  (with-gensyms (trace-opt result success newpos)
    ;; Lookup tracing options in the hash table.
    ;; This actually closes over the symbol `name' so the parsing function remembers which name it was defined with.
    `(let* ((,trace-opt (gethash (symbol-name ',name) *trace-rule*))
            (*trace-recursive* (if (= ,trace-opt 2) t *trace-recursive*))
            (*trace-depth* (if (is-traced ,trace-opt) (1+ *trace-depth*) *trace-depth*)))
       ;; Print trace start
       (when (is-traced ,trace-opt)
         (format t "~v,0T~d: ~a ~{~d~^:~}?~%" (1- *trace-depth*) *trace-depth* ',name ,pos))
       ;; Run the code and intercept the return values
       (multiple-value-bind (,result ,success ,newpos) (progn ,@body)
         ;; Print the end of the trace
         (when (is-traced ,trace-opt)
           ;; Different format depending on success
           (if ,success
               (format t "~v,0T~d: ~a ~{~d~^:~}-~{~d~^:~} -> ~a~%" (1- *trace-depth*) *trace-depth* ',name ,pos ,newpos ,result)
               (format t "~v,0T~d: ~a -|~%" (1- *trace-depth*) *trace-depth* ',name)))
         ;; Return interceptet return values
         (values ,result ,success ,newpos)))))

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
     (when (and ,stack (equal ,pos (first ,stack)))
       (error "Left recursion detected!"))
     ;; Save the position in which this rule was called
     (push (treepos-copy ,pos) ,stack)
     ;; Execute the body.
     (unwind-protect (progn ,@body)
       ;; Make sure the position is popped from the stack /always/.
       (pop ,stack))))

;; defrule macro --------------------------------------------------------------

(defmacro defrule (name lambda-list expr &body options)
  ;; Creates a lambda expression that parses the given grammar rules.
  ;; It then stores the lambda function in the global list *rule-table*,
  ;; therefore the rule functions use a namespace separate from everything
  (with-gensyms (sequence pos oldpos result success last-call-pos)
    ;; Split options into specials, externals and processing options
    (multiple-value-bind (specials externals processing-options)
        (loop for opt in options
           when (or (not (listp opt)) (null opt)) do (error "Illegal option in rule definition for ~a." name)
           when (eql (first opt) :external) append (rest opt) into externals
           when (eql (first opt) :let) append (rest opt) into specials
           when (have (first opt) '(:constant :lambda :destructure :function :identity :flatten :string :vector :test :not)) collect opt into processing-options
           finally (return (values specials externals processing-options)))
      ;; Bind a variable for the following lambda expression to close over
      `(let (,last-call-pos)
         ;; Save the name in the trace rule table
         (setf (gethash (symbol-name ',name) *trace-rule*) 0)
         ;; Save the lambda function in the namespace table
         (setf (gethash ',name *rule-table*)
               ;; Lambda expression that parses according to the given grammar rule
               (lambda (,sequence ,pos ,@lambda-list)
                 ;; Declare special variables specified in the (:external ...) option
                 (declare (special ,@externals))
                 ;; Check for left recursion
                 (with-left-recursion-protection (,pos ,last-call-pos)
                   ;; Bind special variables from the (:let ...) option
                   (with-special-vars-from-options ,specials
                     ;; Save the previous parsing position
                     (let ((,oldpos (treepos-copy ,pos)))
                       ;; Print tracing information
                       (with-tracing (,name ,oldpos)
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
                           (values nil nil ,oldpos))))))))))))

;; Namespace macros -----------------------------------------------------------

(defmacro with-local-rules (&body body)
  ;; Shadow the global rule table with a new rule table
  `(let ((*rule-table* (make-hash-table))
         (*trace-rule* (make-hash-table)))
     ;; Execute the body
     ,@body))
