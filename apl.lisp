;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;			 BASE STRUCTURE				 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass tensor ()
    ((slot-content 
        :accessor tensor-content
        :initarg :init-val))
)

(defclass tensor-lst (tensor) ())

(defclass tensor-scalar (tensor) ())

(defgeneric get-content (object))
(defmethod get-content ((object tensor))
    (slot-value object 'slot-content))

(defun s (arg) (make-instance 'tensor-scalar :init-val arg))

(defun v (&rest args)
    (defun v-aux (lst)
        (if (null lst)
            '()
            (cons (s (car lst)) (v-aux (cdr lst)))))
    (make-instance 'tensor-lst :init-val (v-aux args)))

(defun v-from-lst (lst)
    (make-instance 'tensor-lst :init-val lst))

(defmethod PRINT-OBJECT ((object tensor-scalar) stream)
    (format stream "{~d}" (get-content object)))

(defmethod PRINT-OBJECT ((object tensor-lst) stream)
    (format stream "~a " (get-content object )))



; (defmethod print-tensor ((arg list))
;     (let ((lines 0))
;         (if arg
;             (progn
;                 (setq lines (print-tensor (car arg)))
;                 (if (cdr arg)
;                     (progn
;                         (dotimes (i lines)
;                             (princ #\newline))
;                         (print-tensor (cdr arg))))))
;         (+ lines 1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; CARLOS ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;shape - Creates a vector containing the length of 
;        each dimension of the argument tensor. 
(defgeneric shape (tnsr))
(defmethod shape ((tnsr tensor-scalar))
    NIL)
(defmethod shape ((tnsr tensor-lst))
    (let ((dimension (shape (car (get-content tnsr))))) 
        (if (null dimension)
            (v-from-lst (list (list-length (get-content tnsr))))
            (v-from-lst (cons (list-length (get-content tnsr)) (get-content dimension))))))

;interval - Creates a vector containing an enumeration
;           of all integers starting from 1 up to the
;           argument.
(defgeneric interval (num))
(defmethod interval ((num number))
    (defun recursive-interval (num lst)
        (if (<= num 1)
            (cons (s 1) lst)
            (recursive-interval (- num 1) (cons (s num) lst))))
    (v-from-lst (recursive-interval num '())))

;fold - Accepts a function and returns another function
;       that, given a vector, computes the application
;       of the function to sucessive elements of the vector.
(defgeneric fold (func))
(defmethod fold ((func function))
    (defun fold-aux (vctr)
        (let ((res NIL))
                (dolist (element (get-content vctr))
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
    (defun scan-aux (vctr)
        (let ((last-value NIL)
              (res '()))
                (dolist (element (get-content vctr))
                    (if (null res)
                        (setq last-value element)
                        (setq last-value (funcall func element last-value)))
                    (setq res (append res (list last-value))))
            (v-from-lst res)))
    #'scan-aux)

;outer-product - Accepts a function and returns another
;                functions that, given two tensors, returns
;                a new tensor with the result of applying
;                the function to every combination of values
;                from the first and second tensors.
(defgeneric outer-product (func))
(defmethod outer-product ((func function))
    func)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; REIS ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;drop -	Accepts a scalar n1 or vector (of elements ni) and a non-scalar tensor and
;		returns a tensor where the first (if n > 0) or last (if n < 0) n elements of
;		the i dimension of the tensor were removed.

(defgeneric drop (n1 tensor))

(defmethod drop ((n1 tensor-scalar) (tnsr tensor))
	(let ((n (get-content n1)))
		(cond ((= n 0) tnsr)
			((> n 0) (drop (s (- n 1)) (v-from-lst (rest (get-content tnsr )))))       
			(t (drop (s (+ n 1)) (v-from-lst (butlast (get-content tnsr))))))))

;(defmethod drop ((n1 tensor-lst) (tnsr1 tensor))
;	(let ((n (get-content n1))
;		  (tnsr (get-content tnsr1))
;		  (first-elem (get-content (car n)))
;		  (remaining (rest n)))
;		(cond (and (= (list-length n) 1) (= first-elem 0) tnsr1)
;			((= first-elem 0) (drop remaining tnsr)) ;mas assim vai voltar  remover da primeira dim :(
;			((> first-elem 0) (drop (cons (s (- first-elem 1)) remaining)        )   )

;			)




;		))

;(defun all-equal(lst value)
;	(all-equal-aux lst value T))

;(defun all-equal-aux(lst value bool)

;)


