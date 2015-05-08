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

(defgeneric get-scalar-from-pos (tnsr temp-coords))
(defmethod get-scalar-from-pos ((tnsr tensor-lst) (temp-coords tensor-lst))
    (let ((result tnsr)
            (coords (v-from-lst (nreverse (get-content (hack-dims temp-coords))))))
        (dolist (dim-pos (get-content coords))
            (setq result (nth (1- (get-content dim-pos)) (get-content result))))
        result))

(defgeneric set-scalar-from-pos (tnsr temp-coords scalar))
(defmethod set-scalar-from-pos ((tnsr tensor-lst) (temp-coords tensor-lst) (scalar tensor-scalar))
    (let ((result tnsr)
            (coords (v-from-lst (nreverse (get-content (hack-dims temp-coords))))))
        (dolist (dim-pos (get-content coords))
            (setq result (nth (1- (get-content dim-pos)) (get-content result))))
        (setf (slot-value result 'slot-content) (get-content scalar))
        result))

(defun v-from-lst (lst)
    (make-instance 'tensor-lst :init-val lst))

(defmethod PRINT-OBJECT ((object tensor-scalar) stream)
    (format stream "{~d}" (get-content object)))

(defmethod PRINT-OBJECT-STRING ((object tensor-scalar))
    (cons 0 (concatenate 'string "{" (write-to-string (get-content object)) "} ")))

(defmethod PRINT-OBJECT-STRING ((arg tensor-lst))
    (let ((temp NIL)
        (lines 0)
        (str ""))
        (if arg
            (progn
                (setq temp (PRINT-OBJECT-STRING (car (get-content arg))))
                (setq lines (car temp))
                (setq str (concatenate 'string str (cdr temp)))
                (if (cdr (get-content arg))
                    (progn
                        (dotimes (i lines)
                            (setq str (concatenate 'string str "~%")))
                        (setq str (concatenate 'string str (cdr (PRINT-OBJECT-STRING (v-from-lst (cdr (get-content arg))))))) ))))
        (cons (+ lines 1) str)))

(defmethod PRINT-OBJECT ((object tensor-lst) stream)
    (format stream (cdr (PRINT-OBJECT-STRING object))))

(defmethod hack-dims ((dims tensor-lst))
    (let ((lst (get-content dims)))
        (v-from-lst (if (> (list-length lst) 1)
            (list* (second lst) (first lst) (cddr lst))
            lst))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; CARLOS ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;shape - Creates a vector containing the length of 
;        each dimension of the argument tensor. 
(defgeneric shape (tnsr))
(defmethod shape ((tnsr tensor-scalar))
    NIL)
(defmethod shape ((tnsr tensor-lst))
    (hack-dims
        (let ((dimension (shape (car (get-content tnsr)))))
            (if (null dimension)
                (v-from-lst (list (s (list-length (get-content tnsr)))))
                (v-from-lst (append (get-content dimension) (list (s (list-length (get-content tnsr))))))))))

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


(defun get-dim-and-n-aux (n_lst n_elm dim stop n_lst_rest)
	(if stop   
		`(,n_lst ,n_elm ,dim)
		(let ((first-elem (car (get-content n_lst))) 
				(remain-els (rest (get-content n_lst)) )
				(cont-first-elem  (get-content (car (get-content n_lst)))))
		(cond ((= cont-first-elem 0) (get-dim-and-n-aux (v-from-lst remain-els) n_elm dim stop (cons first-elem n_lst_rest)))
			  ((not (= cont-first-elem 0)) (get-dim-and-n-aux  (v-from-lst (append n_lst_rest (cons (s 0) remain-els))) cont-first-elem (+ 1 (list-length n_lst_rest)) T '()))))))

(defun get-dim-and-n (n)   ;recebe n1 e devolve novo n1 com next dim a 0, a dim a devolver e o n)
	(get-dim-and-n-aux n 0 0 NIL '()))

(defun remove-lists-dim-aux (lst n dim deep)
	(let ((con-lst (get-content lst)))
	(if (= deep dim)
		(if (> n 0)
			(nthcdr n con-lst)
			(reverse (nthcdr (- 0 n) (reverse con-lst))))
		(map 'list #'(lambda (sub-lst) (v-from-lst (remove-lists-dim-aux sub-lst n dim (+ deep 1)) ) ) con-lst)))) 		

(defun remove-lists-dim(lst n dim) ;remove n ou -n listas na dim d
	(remove-lists-dim-aux lst n dim 1))

(defun all-equal-aux(lst value bool)
	(let ((lst_cont (get-content lst)))
	(if (or (not bool) (= (list-length lst_cont) 0))
		bool 
		(if  (= (get-content (car lst_cont)) value) 
				(all-equal-aux (v-from-lst (rest lst_cont)) value T)
				(all-equal-aux lst value NIL)))))

(defun all-equal(lst value)
	(all-equal-aux lst value T))

;drop -	Accepts a scalar n1 or vector (of elements ni) and a non-scalar tensor and
;		returns a tensor where the first (if n > 0) or last (if n < 0) n elements of
;		the i dimension of the tensor were removed.

(defgeneric drop (n1 tensor))

(defmethod drop ((n1 tensor-scalar) (tnsr tensor))
	(let ((n (get-content n1)))
		(cond ((= n 0) tnsr)
			((> n 0) (drop (s (- n 1)) (v-from-lst (rest (get-content tnsr )))))       
			(t (drop (s (+ n 1)) (v-from-lst (butlast (get-content tnsr))))))))


(defmethod drop ((n1 tensor-lst) (tnsr1 tensor))
		(if (all-equal n1 0)
			tnsr1
			(let* ((new_dim_n (get-dim-and-n n1))
					(new-n1 (car new_dim_n))
					(n_els (second new_dim_n))
					(dim (third new_dim_n))
					(new-tnsr (v-from-lst (remove-lists-dim tnsr1 n_els dim))))
				(drop new-n1 new-tnsr))))

;reshape - Returns a tensor with the dimensions refered in the first argument,
;          whose elements are taken from the second argument, repeating them if
;          necessary to fill the resulting tensor

(defun rotate (vec)
	(let ((vec-c (get-content vec)))
  (v-from-lst (append (rest vec-c) (list (first vec-c))))))

(defgeneric reshape (dims fill-data))

(defvar fill-data-var)

(defun get-first-fill-data()
	(let ((el (first (get-content fill-data-var))))
		(progn (setf fill-data-var (rotate fill-data-var))
				el)))

(defun reshape-aux (dims)
	(if (= (list-length (get-content dims)) 0)
		(get-first-fill-data)		
		(let ((dim-qty  (get-content (car (last (get-content dims)))))
			  (rest-dim (butlast (get-content dims))))
			  (v-from-lst (loop for i from 1 to dim-qty 
			  					collect (reshape-aux (v-from-lst rest-dim)))))))

(defmethod reshape ((dims tensor-lst) (fill-data tensor-lst))
    (progn (setf  fill-data-var fill-data)
        (reshape-aux (hack-dims dims))))









