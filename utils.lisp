(in-package :parseq)

;; Macro utilities

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym (concatenate 'string (symbol-name ',n) "-"))))
     ,@body))

;; List functions

(defun mklist (item)
  (if (listp item) item (list item)))

(defun have (item sequence &key (test #'eql) (key #'identity))
  ;; Checks whether the given item is in the list
  (some #'(lambda (x) (funcall test item (funcall key x))) sequence))

(defun flatten (x)
  "Flattens the given tree, x, into a list."
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun l< (list length)
  "Tests efficiently whether the length of the list is smaller than the given length."
  (cond
    ((null list) (plusp length))
    ((zerop length) nil)
    (t (l< (rest list) (- length 1)))))

(defun l>= (list length)
  "Tests efficiently whether the length of the list is greater or equal to the given length."
  (not (l< list length)))

(defun l> (list length)
  "Tests efficiently whether the length of the list is greater than the given length."
  (cond
    ((null list) nil)
    ((zerop length) (consp list))
    (t (l> (rest list) (- length 1)))))

(defun l<= (list length)
  "Tests efficiently whether the length of the list is smaller or equal to the given length."
  (not (l> list length)))

(defun l= (list length)
  "Tests efficiently whether the length of the list is equal to the given length."
  (cond
    ((null list) (= 0 length))
    ((zerop length) (null list))
    (t (l= (rest list) (- length 1)))))

(defmacro appendf (list &rest items)
  "Appends the given item to the list using setf."
  `(setf ,list (append ,list (list ,@items))))

(defun last-1 (list)
  "Retrieves the last item from the given list."
  (car (last list)))

;; Symbol functions

(defun symbol= (a b)
  (and (symbolp a) (symbolp b) (string= (symbol-name a) (symbol-name b))))

;; Control flow macros

(defmacro case-test ((keyform &key (test 'eql)) &body clauses)
  (with-gensyms (key)
      `(let ((,key ,keyform))
         (cond ,@(loop for clause in clauses collect
                      (if (eql (first clause) t)
                          clause
                          `((,test ,key ,(if (symbolp (first clause)) `(quote ,(first clause)) (first clause))) ,@(rest clause))))))))

;; Generic sequence functions

(defun sequencep (object)
  (or (listp object) (vectorp object)))

(defun sequence= (seq-a seq-b &key (start1 0) (start2 0) end1 end2 (test #'eql) (key #'identity))
  (let ((a (subseq seq-a start1 end1))
        (b (subseq seq-b start2 end2)))
    (when (and (= (length a) (length b)) (equal (type-of a) (type-of b)))
      (loop for i below (length a) always (funcall test (funcall key (elt a i)) (funcall key (elt b i)))))))

(defun subseq-at (subseq seq pos)
  (let ((len (length subseq)))
    ;; Ensure seq is long enough
    (when (<= len (- (length seq) pos))
      (sequence= subseq seq :start2 pos :end2 (+ pos len)))))

(defun unsigned-byte-p (object)
  (and (integerp object) (>= object 0) (<= object 255)))

;; Logic functions

(defmacro or2 (&rest forms)
  ;; Works like or, but operates on the second value returned by each form.
  (when forms
      (with-gensyms (result success)
        `(multiple-value-bind (,result ,success) ,(first forms)
           (if ,success
               (values ,result ,success)
               (or2 ,@(rest forms)))))))

(defmacro or2-exclusive ((exclusion-list) &rest forms)
  ;; Like or2, but checks whether form is excluded in exclusion list before evaluating form.
  ;; Also returns the  index of the form that succeeded
  (with-gensyms (result success blockname)
    `(block ,blockname
       ,@(loop for form in forms for i upfrom 0 collect
            ;; Check for exclusion
              `(unless (nth ,i ,exclusion-list)
                 ;; Form is not excluded, try it
                 (multiple-value-bind (,result ,success) ,(nth i forms)
                   (when ,success
                     ;; Return the result, the success, plus the index which succeeded
                     (return-from ,blockname (values ,result ,success ,i)))))))))

;; Random function (for testing)

(defun shuffle (list)
  "Creates a new list in which the items of the given list are shuffled"
  ;; Algorithm by Donald Knuth
  (let ((n (list-length list)) (result (copy-list list)))
    (loop for i below (- n 1) do
         (rotatef (nth i result) (nth (+ i (random (- n i))) result))
       finally (return result))))

;; String functions

(defun cat (&rest items)
  (apply #'concatenate 'string (loop for i in items collect (cond
                                                              ((stringp i) i)
                                                              ((characterp i) (string i))
                                                              ((symbolp i) (symbol-name i))
                                                              (t (error "Cannot convert ~a into string!" (type-of i)))))))
