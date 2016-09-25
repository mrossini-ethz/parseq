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

(defun quoted-symbol-p (x)
  (and (listp x) (l= x 2) (eql (first x) 'quote) (symbolp (second x))))

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
  (with-gensyms (result success blockname excl)
    ;; Evaluate exclusion-list only once
    `(let ((,excl ,exclusion-list))
       ;; Create a block to return from
       (block ,blockname
         ;; Try each form in succession
         ,@(loop for form in forms for i upfrom 0 collect
              ;; Check for exclusion
                `(unless (nth ,i ,excl)
                   ;; Form is not excluded, try it
                   (multiple-value-bind (,result ,success) ,(nth i forms)
                     (when ,success
                       ;; Return the result, the success, plus the index which succeeded
                       (return-from ,blockname (values ,result ,success ,i))))))))))

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
                                                              ((unsigned-byte-p i) (string (code-char i)))
                                                              (t (error "Cannot convert ~a into string!" (type-of i)))))))

;; Tree position functions

(defun treepos-valid (pos tree)
  (when (and (sequencep tree) (not (minusp (first pos))))
    (if (l> pos 1)
        ;; Not toplevel
        (when (> (length tree) (first pos))
          ;; Descend into the toplevel item to recursively check the sublevel
          (treepos-valid (rest pos) (elt tree (first pos))))
        ;; Toplevel. Check whether the list is longer than the position index.
        (> (length tree) (first pos)))))

(defun treeitem (pos tree)
  (when (and (sequencep tree) (listp pos))
    (cond
      ((l> pos 1) (treeitem (rest pos) (elt tree (first pos))))
      ((null pos) tree)
      (t (elt tree (first pos))))))

(defun treepos-length (pos tree)
  (if (sequencep tree)
     (if (l> pos 1)
         (treepos-length (rest pos) (nth (first pos) tree))
         (and (sequencep (elt tree (first pos))) (length (elt tree (first pos)))))
     (error "Attempting to descend into a non-sequence type.")))

(defun treepos-step (pos &optional (delta 1))
  (let ((newpos (copy-tree pos)))
    (incf (car (last newpos)) delta)
    newpos))

(defun treepos-copy (pos)
  (copy-tree pos))

;; Hash table functions

(defun copy-hash-table (hash-table)
  ;; Creates a copy of the given hash table
  (loop for k being the hash-keys in hash-table using (hash-value v)
     with ht = (make-hash-table)
     do (setf (gethash k ht) v)
     finally (return ht)))
