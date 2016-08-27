(in-package :parser)

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

(defun symbol= (a b)
  (and (symbolp a) (symbolp b) (string= (symbol-name a) (symbol-name b))))

(defmacro case-test ((keyform &key (test 'eql)) &body clauses)
  (with-gensyms (key)
      `(let ((,key ,keyform))
         (cond ,@(loop for clause in clauses collect
                      (if (eql (first clause) t)
                          clause
                          `((,test ,key ,(if (symbolp (first clause)) `(quote ,(first clause)) (first clause))) ,@(rest clause))))))))
