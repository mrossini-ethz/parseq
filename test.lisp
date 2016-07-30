(require :asdf)
(push #p"./" asdf:*central-registry*)
(asdf:load-system :parser)
(use-package :parser)

(defvar *test-name* nil)
(defvar *test-failures* 0)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym (concatenate 'string (symbol-name ',n) "-"))))
     ,@body))

(defmacro condition= (form condition)
  "Tests whether the execution of the form results in the given condition (returning T). If no condition or a different condition occurs, NIL is returned."
  `(handler-case (and ,form nil)
     (,condition () t)
     (t () nil)))

(defmacro define-test (name parameters &body body)
  "Define a test function. Within a test function we can call
   other test functions or use 'check' to run individual test
   cases."
  `(defun ,name ,parameters
     (when (zerop (list-length *test-name*))
       (setf *test-failures* 0))
     (let ((*test-name* (append *test-name* (list ',name))) (test-failures-save *test-failures*))
       (format t "~V<~>Testing ~{~a~^:~} ...~%" (- (list-length *test-name*) 1) *test-name*)
       ,@body
       (if (> *test-failures* test-failures-save)
           (progn
             (format t "~V<~>Total number of tests failed in ~{~a~^:~}: ~a~%" (- (list-length *test-name*) 1) *test-name* (- *test-failures* test-failures-save))
             (decf *test-failures*))
           t))))

(defun report-result (result form expanded-form)
  "Report the results of a single test case. Called by 'check'."
  (when (not result)
    (incf *test-failures*)
    (format t "~V<~> ~:[Failed~;Passed~]: ~s~@[ => ~*~s~]~%" (- (list-length *test-name*) 1) result form (not (equal form expanded-form)) expanded-form))
  result)

(defmacro combine-results (&body forms)
  "Logical AND operation of the given forms, but without short-circuiting. This ensures that each form is evaluated exactly once."
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

(defmacro check (&body forms)
  "Run each expression in 'forms' once and reports whether succeded (t) or failed (nil)."
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f ,@(if (and (listp f) (not (eql 'condition= (first f)))) `((list ',(first f) ,@(rest f))) `(',f))))))

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

(defun xnor (&rest forms)
  (evenp (count-if #'identity forms)))

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
