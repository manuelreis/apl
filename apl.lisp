;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;			 BASE STRUCTURE				 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(defclass tensor ()
;	((slot-content :initform (make-array 0)
;				 :accessor tensor-content
;				 :initarg :init-val))
;	)

;(defun s (arg) (make-instance 'tensor :init-val `#(,arg)))

;(defun v (&rest args) args (make-instance 'tensor :init-val (make-array (list-length args) :initial-contents args)))

(defun s (arg) `(,arg))

(defun v (&rest args) args)

(defgeneric print-tensor (arg))

(defmethod print-tensor ((arg number))
    (progn
        (princ (write-to-string arg))
        (princ " ")
    0))

(defmethod print-tensor ((arg list))
    (let ((lines 0))
        (if arg
            (progn
                (setq lines (print-tensor (car arg)))
                (if (cdr arg)
                    (progn
                        (dotimes (i lines)
                            (princ #\newline))
                        (print-tensor (cdr arg))))))
        (+ lines 1)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; CARLOS ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;shape - Creates a vector containing the length of 
;        each dimension of the argument tensor. 
(defgeneric shape (tnsr))
(defmethod shape ((tnsr number))
    NIL)
(defmethod shape ((tnsr list))
    (cons (list-length tnsr) (shape (car tnsr))))

;interval - Creates a vector containing an enumeration
;           of all integers starting from 1 up to the
;           argument.
(defgeneric interval (num))
(defmethod interval ((num number))
    (defun recursive-interval (num lst)
        (if (<= num 1)
            (cons 1 lst)
            (recursive-interval (- num 1) (cons num lst))))
    (recursive-interval num '()))

;fold - Accepts a function and returns another function
;       that, given a vector, computes the application
;       of the function to sucessive elements of the vector.
(defgeneric fold (func))
(defmethod fold ((func function))
    (defun fold-aux (lst)
        (let ((res NIL))
                (dolist (element lst res)
                    (if (null res)
                        (setq res element)
                        (setq res (funcall func element res))))
            res))
    #'fold-aux)

;scan - Similar to fold but using increasingly large
;       subsets of the elements of the vector, starting
;       from a subset containing just the first element
;       up to a subset containing all elements. 
(defgeneric scan (func))
(defmethod scan ((func function))
    (defun scan-aux (lst)
        (let ((last-value NIL)
              (res '()))
                (dolist (element lst)
                    (if (null res)
                        (setq last-value element)
                        (setq last-value (funcall func element last-value)))
                    (setq res (append res (list last-value))))
            res))
    #'scan-aux)

;outer-product - Accepts a function and returns another
;                functions that, given two tensors, returns
;                a new tensor with the result of applying
;                the function to every combination of values
;                from the first and second tensors.
(defgeneric outer-product (func))
(defmethod outer-product ((func function))
    func)


;apply-fun - Accepts a function and returns another function
;            that, given a tensor, applies the function to each
;            element
(defgeneric apply-func (func))
(defmethod apply-func ((func function))
    (defun apply-func-aux (lst)
        (cond
            ((null lst) lst)
            ((listp lst) (cons (apply-func-aux (car lst)) (apply-func-aux (cdr lst))))
            (T (funcall func lst))))
    #'apply-func-aux)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;