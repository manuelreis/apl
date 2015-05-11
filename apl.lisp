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

(defmethod s ((arg number)) (make-instance 'tensor-scalar :init-val arg))

(defun v (&rest args)
    (defun v-aux (lst)
        (if (null lst)
            '()
            (cons (s (car lst)) (v-aux (cdr lst)))))
    (make-instance 'tensor-lst :init-val (v-aux args)))

(defmethod hard-tensor-copy ((tnsr tensor-scalar))
    (s (get-content tnsr)))

(defmethod hard-tensor-copy ((tnsr tensor-lst))
    (defun hard-tensor-copy-recursive (lst)
        (if (null lst)
            lst
            (cons (hard-tensor-copy (car lst)) (hard-tensor-copy-recursive (cdr lst)))))
    (let ((v-tnsr (tensor-to-vector tnsr)))
        (reshape
            (shape tnsr)
            (v-from-lst (hard-tensor-copy-recursive (get-content v-tnsr))))))

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

(defmethod tensor-to-vector ((tnsr tensor-lst))
    (v-from-lst (tensor-to-vector-aux tnsr)))
(defmethod tensor-to-vector-aux ((tnsr tensor-scalar))
    (list tnsr))
(defmethod tensor-to-vector-aux ((tnsr tensor-lst))
    (let ((temp NIL)
        (lst NIL))
        (if tnsr
            (progn
                (setq temp (tensor-to-vector-aux (car (get-content tnsr))))
                (setq lst (append lst temp))
                (if (cdr (get-content tnsr))
                    (progn
                        (setq lst (append lst (tensor-to-vector-aux (v-from-lst (cdr (get-content tnsr)))))) ))))
        lst))

(defmethod hack-dims ((dims tensor-lst))
    (let ((lst (get-content dims)))
        (v-from-lst (if (> (list-length lst) 1)
            (list* (second lst) (first lst) (cddr lst))
            lst))))

;queria isto mas vendo a class! é possivel?
(defun tnsr-lst? (tnsr)
    (if (eq (type-of tnsr) 'tensor-lst)
        t
        nil))

(defun tnsr-scalar? (tnsr)
    (if (eq (type-of tnsr) 'tensor-scalar)
        t
        nil))

(defun flatten (mylist)
  (cond
   ((null mylist) nil)
   ((atom mylist) (list mylist))
   (t
    (append (flatten (car mylist)) (flatten (cdr mylist))))))

(defun remove-nth (n list)
  (declare
    (type (integer 0) n)
    (type list list))
  (if (or (zerop n) (null list))
    (cdr list)
    (cons (car list) (remove-nth (1- n) (cdr list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; CARLOS ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;shape - Creates a vector containing the length of 
;        each dimension of the argument tensor. 
(defgeneric shape (tnsr))
(defmethod shape ((tnsr tensor-scalar))
    NIL)
(defmethod shape-aux ((tnsr tensor-scalar))
    NIL)
(defmethod shape-aux ((tnsr tensor-lst))
    (let ((dimension (shape-aux (car (get-content tnsr)))))
            (if (null dimension)
                (v-from-lst (list (s (list-length (get-content tnsr)))))
                (v-from-lst (append (get-content dimension) (list (s (list-length (get-content tnsr)))))))))
(defmethod shape ((tnsr tensor-lst))
    (hack-dims (shape-aux tnsr)))

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
    (defun outer-product-aux (tnsr1 tnsr2)
        (let ((shape1 (get-content (shape tnsr1)))
                (shape2 (get-content (shape tnsr2)))
                (v-tnsr1 (tensor-to-vector tnsr1))
                (v-tnsr2 (tensor-to-vector tnsr2))
                (result NIL))
            (dolist (element1 (get-content v-tnsr1))
                (dolist (element2 (get-content v-tnsr2))
                    (setq result (append result (list (funcall func element1 element2))))))
            (reshape (v-from-lst (append shape2 shape1)) (v-from-lst result))))
    #'outer-product-aux)

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

(defun dim-rescale (d num-dims)
	(+ (- num-dims d) 1))

;drop -	Accepts a scalar n1 or vector (of elements ni) and a non-scalar tensor and
;		returns a tensor where the first (if n > 0) or last (if n < 0) n elements of
;		the i dimension of the tensor were removed.

(defgeneric drop (n1 tensor))

(defmethod drop ((n1 tensor-scalar) (tnsr tensor))
	(let ((n (get-content n1)))
		(cond ((= n 0) tnsr)
			((> n 0) (drop (s (- n 1)) (v-from-lst (rest (get-content tnsr )))))       
			(t (drop (s (+ n 1)) (v-from-lst (butlast (get-content tnsr))))))))

(defun drop-aux (n1 tnsr1)
	(if (all-equal n1 0)
				tnsr1
				(let* ((new_dim_n (get-dim-and-n n1))
						(new-n1 (car new_dim_n))
						(n_els (second new_dim_n))
						(dim (third new_dim_n))
						(dim-rescaled (dim-rescale dim (list-length (get-content new-n1))))
						(new-tnsr (v-from-lst (remove-lists-dim tnsr1 n_els dim-rescaled))))
					(drop-aux new-n1 new-tnsr))))

(defmethod drop ((n1 tensor-lst) (tnsr1 tensor))
	(drop-aux (hack-dims n1) tnsr1))



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


;catenate - If the two arguments are scalars, returns a vector containing those
;			arguments. If the two arguments are tensors, returns a tensor that joins
;			the arguments along the their last dimension.


(defgeneric catenate (arg1 arg2))

(defun catenate-aux (arg1 arg2)
	(let ((arg1-c (get-content arg1))
		  (arg2-c (get-content arg2)))
		(if (= (list-length arg1-c) 1)
			(v-from-lst (append (get-content (car arg1-c))  (get-content (car arg2-c)) ))
			(v-from-lst (append (list (v-from-lst (append (get-content (car arg1-c)) (get-content (car arg2-c)))) 
									  (catenate-aux  (v-from-lst (rest arg1-c)) (v-from-lst (rest arg2-c)))))))))

(defmethod catenate ((arg1 tensor-lst) (arg2 tensor-lst))
	(let ((arg1-c (get-content arg1))
		  (arg2-c (get-content arg2)))
			(if (= (list-length (get-content (shape arg1))) 2)
				(catenate-aux arg1 arg2)
				(v-from-lst (append arg1-c arg2-c)))))


;member? - Returns a tensor of booleans with the same shape and dimension of the
; 			first argument, containing 1 for each element in the corresponding location
; 			in the first argument that occurs somewhere in the second argument and
; 			0 otherwise.



(defun generate-all-pos-from-shape-aux (shp pos)
	(if (= 0 (list-length shp))
		(v-from-lst pos) 
		(let ((limit (get-content (car shp))))
				 	(loop for pos_i in (get-content (interval limit)) 
						collect (generate-all-pos-from-shape-aux (rest shp) (append pos (list pos_i)))))))

(defun generate-all-pos-from-shape (shp)
	(let ((shp-ct (get-content shp)))
		(flatten (generate-all-pos-from-shape-aux shp-ct '()))))


(defgeneric member? (tnsr elems))
 
(defmethod member? ((tnsr tensor-scalar) (elems tensor-lst)) 
	(let* ((elems-vec (get-content (tensor-to-vector elems)))
		  (rest-elems-vec (rest elems-vec)))
			(if (= (get-content tnsr) (get-content (car elems-vec)))
				1
				(if (= (list-length rest-elems-vec) 0)
					0
					(member? tnsr (v-from-lst rest-elems-vec))))))


(defmethod member? ((tnsr tensor-lst) (elems tensor-lst))
	(let ((new-tnsr (hard-tensor-copy tnsr))
		 (shp (shape tnsr)))
			(dolist (pos (generate-all-pos-from-shape shp))
		 		(let* ((result (get-scalar-from-pos tnsr pos))
		 			  (is-member (s (member? result elems))))
		 			(set-scalar-from-pos new-tnsr pos is-member)))
				new-tnsr))

;select - From a tensor of booleans and another tensor, returns a tensor containing
;  		  only the elements of the last dimension of the second argument whose
; 		  corresponding element in the first tensor is 1.


(defun new-set-elems-and-index-to-remove-aux (elems new-elems index)
	(if (= 0 (get-content (car elems)))
		(list (v-from-lst (flatten (append (list new-elems) (rest elems)))) index)
		(new-set-elems-and-index-to-remove-aux (rest elems) (append (list new-elems) (car elems)) (+ 1 index))))

(defun new-set-elems-and-index-to-remove (elems)
	(new-set-elems-and-index-to-remove-aux (get-content elems) '() 1))

(defun select-columns (tnsr index)
	(v-from-lst (map 'list #'(lambda (col) (v-from-lst (remove-nth (- index 1) (get-content col)))) (get-content tnsr))))

(defun select-specific (tnsr index)
	(if (= (list-length (get-content (shape tnsr))) 2)
		(select-columns tnsr index)
		(v-from-lst (remove-nth (- index 1) (get-content tnsr)))))

(defun select (elems tnsr)
	(cond  ((all-equal elems 1) tnsr)
		   ((all-equal elems 0)  NIL)
		 	(t (let* ((elems-and-idx (new-set-elems-and-index-to-remove elems))
			   (new-elems (car elems-and-idx))
			   (index (second elems-and-idx))
			   (new-tnsr  (select-specific tnsr index)))
					(if (all-equal new-elems 1)
						new-tnsr
						(select-general new-elems new-tnsr))))))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; ANA RITA DA COSTA PEREIRA ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;; MONADIC FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;
(defun .- (arg1 &optional arg2)
    (if (null arg2)
        (.-Monadic arg1)
        (.-Dyadic arg1 arg2)))

(defun ./ (arg1 &optional arg2)
    (if (null arg2)
        (./Monadic arg1)
        (./Dyadic arg1 arg2)))

(defun monadic-tns (func arg)
    (if (null arg)
        (list)
        (cons (funcall func (car arg)) (monadic-tns func (cdr arg)))))

;.- Creates a tensor whose elements are the symmetric 
;of the corresponding elements of the argument tensor. 
(defgeneric .-Monadic (arg))
(defmethod .-Monadic ((arg tensor-scalar))
    (make-instance 'tensor-scalar :init-val (- 0 (get-content arg))))

(defmethod .-Monadic ((arg tensor-lst))
    (make-instance 'tensor-lst :init-val (monadic-tns #'.-Monadic (get-content arg))))


;./ Same as the previous one, but using the inverse. 
(defgeneric ./Monadic (arg))
(defmethod ./Monadic ((arg tensor-scalar))
    (make-instance 'tensor-scalar :init-val (/ 1 (get-content arg))))

(defmethod ./Monadic ((arg tensor-lst))
    (make-instance 'tensor-lst :init-val (monadic-tns #'./ (get-content arg))))

;.! Same as the previous one, but using the factorial.
(defgeneric .! (arg))
(defmethod .! ((arg tensor-scalar))
    (make-instance 'tensor-scalar :init-val (! (get-content arg))))

(defmethod .! ((arg tensor-lst))
    (make-instance 'tensor-lst :init-val (monadic-tns #'.! (get-content arg))))

;.sin Same as the previous one, but using the sin function. 
(defgeneric .sin (arg))
(defmethod .sin ((arg tensor-scalar))
    (make-instance 'tensor-scalar :init-val (sin (get-content arg))))

(defmethod .sin ((arg tensor-lst))
    (make-instance 'tensor-lst :init-val (monadic-tns #'.sin (get-content arg))))


;.cos Same as the previous one, but using the cos function. 
(defgeneric .cos (arg))
(defmethod .cos ((arg tensor-scalar))
    (make-instance 'tensor-scalar :init-val (cos (get-content arg))))

(defmethod .cos ((arg tensor-lst))
    (make-instance 'tensor-lst :init-val (monadic-tns #'.cos (get-content arg))))


;.not Same as the previous one, but using the negation. 
;The result is a tensor containing, as elements, the integers 
;0 or 1, depending on the corresponding element in the 
;argument tensor being different that zero or equal to zero.
(defgeneric .not (arg))
(defmethod .not ((arg tensor-scalar))
    (if (= (get-content arg) 0)
        (make-instance 'tensor-scalar :init-val 1)
        (make-instance 'tensor-scalar :init-val 0)))

(defmethod .not ((arg tensor-lst))
    (make-instance 'tensor-lst :init-val (monadic-tns #'.not (get-content arg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;; DYATIC FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dyadic-tns-tns (func lst1 lst2)
    (if (null lst1)
        (list)
        (cons (funcall func (car lst1) (car lst2)) (dyadic-tns-tns func (cdr lst1) (cdr lst2)))))

(defun dyadic-tns-scalar (func int lst)
    (if (null lst)
        (list)
        (cons (funcall func int (car lst)) (dyadic-tns-scalar func int (cdr lst)))))

(defun dyadic-equal-dim (lst1 lst2)
    (if (null lst1)
        t
        (progn
            (if (= (get-content (car lst1)) (get-content (car lst2)))
                (dyadic-equal-dim (cdr lst1) (cdr lst2))
                NIL)))) 


;.+ Creates a tensor with the sum of the corresponding 
;elements of the argument tensors.
(defgeneric .+ (arg1 arg2))
(defmethod .+ ((arg1 tensor-scalar) (arg2 tensor-scalar))
    (make-instance 'tensor-scalar :init-val (+ (get-content arg1) (get-content arg2))))

(defmethod .+ ((arg1 tensor-scalar) (arg2 tensor-lst))
    (make-instance 'tensor-lst :init-val (dyadic-tns-scalar #'.+ arg1 (get-content arg2))))

(defmethod .+ ((arg1 tensor-lst) (arg2 tensor-scalar))
    (make-instance 'tensor-lst :init-val (dyadic-tns-scalar #'.+ arg2 (get-content arg1))))

(defmethod .+ ((arg1 tensor-lst) (arg2 tensor-lst))
    (if (not (null (dyadic-equal-dim (get-content (shape arg1)) (get-content (shape arg2)))))
        (make-instance 'tensor-lst :init-val (dyadic-tns-tns #'.+ (get-content arg1) (get-content arg2)))
        nil))


;.- Same as the previous one, but using subtraction.
(defgeneric .-Dyadic (arg1 arg2))
(defmethod .-Dyadic ((arg1 tensor-scalar) (arg2 tensor-scalar))
    (make-instance 'tensor-scalar :init-val (- (get-content arg1) (get-content arg2))))

(defmethod .-Dyadic ((arg1 tensor-scalar) (arg2 tensor-lst))
    (make-instance 'tensor-lst :init-val (dyadic-tns-scalar #'.-Dyadic arg1 (get-content arg2))))

(defmethod .-Dyadic ((arg1 tensor-lst) (arg2 tensor-scalar))
    (make-instance 'tensor-lst :init-val (dyadic-tns-scalar #'.-Dyadic arg2 (get-content arg1))))

(defmethod .-Dyadic ((arg1 tensor-lst) (arg2 tensor-lst))
    (if (not (null (dyadic-equal-dim (get-content (shape arg1)) (get-content (shape arg2)))))
        (make-instance 'tensor-lst :init-val (dyadic-tns-tns #'.-Dyadic (get-content arg1) (get-content arg2)))
        nil))


;.* Same as the previous one, but using multiplication. 
(defgeneric .* (arg1 arg2))
(defmethod .* ((arg1 tensor-scalar) (arg2 tensor-scalar))
    (make-instance 'tensor-scalar :init-val (* (get-content arg1) (get-content arg2))))

(defmethod .* ((arg1 tensor-scalar) (arg2 tensor-lst))
    (make-instance 'tensor-lst :init-val (dyadic-tns-scalar #'.* arg1 (get-content arg2))))

(defmethod .* ((arg1 tensor-lst) (arg2 tensor-scalar))
    (make-instance 'tensor-lst :init-val (dyadic-tns-scalar #'.* arg2 (get-content arg1))))

(defmethod .* ((arg1 tensor-lst) (arg2 tensor-lst))
    (if (not (null (dyadic-equal-dim (get-content (shape arg1)) (get-content (shape arg2)))))
        (make-instance 'tensor-lst :init-val (dyadic-tns-tns #'.* (get-content arg1) (get-content arg2)))
        nil))

;./ Same as the previous one, but using division. 
(defgeneric ./Dyadic (arg1 arg2))
(defmethod ./Dyadic ((arg1 tensor-scalar) (arg2 tensor-scalar))
   (make-instance 'tensor-scalar :init-val (/ (get-content arg1) (get-content arg2))))

(defmethod ./Dyadic ((arg1 tensor-scalar) (arg2 tensor-lst))
    (make-instance 'tensor-lst :init-val (dyadic-tns-scalar #'./Dyadic arg1 (get-content arg2))))

(defmethod ./Dyadic ((arg1 tensor-lst) (arg2 tensor-scalar))
    (make-instance 'tensor-lst :init-val (dyadic-tns-scalar #'./Dyadic arg2 (get-content arg1))))

(defmethod ./Dyadic ((arg1 tensor-lst) (arg2 tensor-lst))
    (if (not (null (dyadic-equal-dim (get-content (shape arg1)) (get-content (shape arg2)))))
        (make-instance 'tensor-lst :init-val (dyadic-tns-tns #'./Dyadic (get-content arg1) (get-content arg2)))
        nil))


;.// Same as the previous one, but using integer division. 
(defgeneric .// (arg1 arg2))
(defmethod .// ((arg1 tensor-scalar) (arg2 tensor-scalar))
    (make-instance 'tensor-scalar :init-val (floor (get-content arg1) (get-content arg2))))

(defmethod .// ((arg1 tensor-scalar) (arg2 tensor-lst))
    (make-instance 'tensor-lst :init-val (dyadic-tns-scalar #'.// arg1 (get-content arg2))))

(defmethod .// ((arg1 tensor-lst) (arg2 tensor-scalar))
    (make-instance 'tensor-lst :init-val (dyadic-tns-scalar #'.// arg2 (get-content arg1))))

(defmethod .// ((arg1 tensor-lst) (arg2 tensor-lst))
    (if (not (null (dyadic-equal-dim (get-content (shape arg1)) (get-content (shape arg2)))))
        (make-instance 'tensor-lst :init-val (dyadic-tns-tns #'.// (get-content arg1) (get-content arg2)))
        nil))


;.% Same as the previous one, but using the remainder of 
;the integer division. 
(defgeneric .% (arg1 arg2))
(defmethod .% ((arg1 tensor-scalar) (arg2 tensor-scalar))
    (make-instance 'tensor-scalar :init-val (nth-value 1 (floor (get-content arg1) (get-content arg2)))))

(defmethod .% ((arg1 tensor-scalar) (arg2 tensor-lst))
    (make-instance 'tensor-lst :init-val (dyadic-tns-scalar #'.% arg1 (get-content arg2))))

(defmethod .% ((arg1 tensor-lst) (arg2 tensor-scalar))
    (make-instance 'tensor-lst :init-val (dyadic-tns-scalar #'.% arg2 (get-content arg1))))

(defmethod .% ((arg1 tensor-lst) (arg2 tensor-lst))
    (if (not (null (dyadic-equal-dim (get-content (shape arg1)) (get-content (shape arg2)))))
        (make-instance 'tensor-lst :init-val (dyadic-tns-tns #'.% (get-content arg1) (get-content arg2)))
        nil))


;.< Same as the previous one, but using the relation “less than.”
;The result tensor will have, as elements, the integers 0 or 1.
(defgeneric .< (arg1 arg2))
(defmethod .< ((arg1 tensor-scalar) (arg2 tensor-scalar))
    (if (< (get-content arg1) (get-content arg2))
        (make-instance 'tensor-scalar :init-val 1)
        (make-instance 'tensor-scalar :init-val 0)))

(defmethod .< ((arg1 tensor-scalar) (arg2 tensor-lst))
    (make-instance 'tensor-lst :init-val (dyadic-tns-scalar #'.< arg1 (get-content arg2))))

(defmethod .< ((arg1 tensor-lst) (arg2 tensor-scalar))
    (make-instance 'tensor-lst :init-val (dyadic-tns-scalar #'.< arg2 (get-content arg1))))

(defmethod .< ((arg1 tensor-lst) (arg2 tensor-lst))
    (if (not (null (dyadic-equal-dim (get-content (shape arg1)) (get-content (shape arg2)))))
        (make-instance 'tensor-lst :init-val (dyadic-tns-tns #'.< (get-content arg1) (get-content arg2)))
        nil))


;.> Same as the previous one, but using the relation “greater than.”
(defgeneric .> (arg1 arg2))
(defmethod .> ((arg1 tensor-scalar) (arg2 tensor-scalar))
    (if (> (get-content arg1) (get-content arg2))
        (make-instance 'tensor-scalar :init-val 1)
        (make-instance 'tensor-scalar :init-val 0)))

(defmethod .> ((arg1 tensor-scalar) (arg2 tensor-lst))
    (make-instance 'tensor-lst :init-val (dyadic-tns-scalar #'.> arg1 (get-content arg2))))

(defmethod .> ((arg1 tensor-lst) (arg2 tensor-scalar))
    (make-instance 'tensor-lst :init-val (dyadic-tns-scalar #'.> arg2 (get-content arg1))))

(defmethod .> ((arg1 tensor-lst) (arg2 tensor-lst))
    (if (not (null (dyadic-equal-dim (get-content (shape arg1)) (get-content (shape arg2)))))
        (make-instance 'tensor-lst :init-val (dyadic-tns-tns #'.> (get-content arg1) (get-content arg2)))
        nil))


;.<= Same as the previous one, but using the relation “less 
;than or equal to.” 
(defgeneric .<= (arg1 arg2))
(defmethod .<= ((arg1 tensor-scalar) (arg2 tensor-scalar))
    (if (<= (get-content arg1) (get-content arg2))
        (make-instance 'tensor-scalar :init-val 1)
        (make-instance 'tensor-scalar :init-val 0)))

(defmethod .<= ((arg1 tensor-scalar) (arg2 tensor-lst))
    (make-instance 'tensor-lst :init-val (dyadic-tns-scalar #'.<= arg1 (get-content arg2))))

(defmethod .<= ((arg1 tensor-lst) (arg2 tensor-scalar))
    (make-instance 'tensor-lst :init-val (dyadic-tns-scalar #'.<= arg2 (get-content arg1))))

(defmethod .<= ((arg1 tensor-lst) (arg2 tensor-lst))
    (if (not (null (dyadic-equal-dim (get-content (shape arg1)) (get-content (shape arg2)))))
        (make-instance 'tensor-lst :init-val (dyadic-tns-tns #'.<= (get-content arg1) (get-content arg2)))
        nil))


;.>= Same as the previous one, but using the relation “greater 
;than or equal to.” 
(defgeneric .>= (arg1 arg2))
(defmethod .>= ((arg1 tensor-scalar) (arg2 tensor-scalar))
    (if (>= (get-content arg1) (get-content arg2))
        (make-instance 'tensor-scalar :init-val 1)
        (make-instance 'tensor-scalar :init-val 0)))

(defmethod .>= ((arg1 tensor-scalar) (arg2 tensor-lst))
    (make-instance 'tensor-lst :init-val (dyadic-tns-scalar #'.>= arg1 (get-content arg2))))

(defmethod .>= ((arg1 tensor-lst) (arg2 tensor-scalar))
    (make-instance 'tensor-lst :init-val (dyadic-tns-scalar #'.>= arg2 (get-content arg1))))

(defmethod .>= ((arg1 tensor-lst) (arg2 tensor-lst))
    (if (not (null (dyadic-equal-dim (get-content (shape arg1)) (get-content (shape arg2)))))
        (make-instance 'tensor-lst :init-val (dyadic-tns-tns #'.>= (get-content arg1) (get-content arg2)))
        nil))


;.= Same as the previous one, but using the relation “equal to.” 
(defgeneric .= (arg1 arg2))
(defmethod .= ((arg1 tensor-scalar) (arg2 tensor-scalar))
    (if (= (get-content arg1) (get-content arg2))
        (make-instance 'tensor-scalar :init-val 1)
        (make-instance 'tensor-scalar :init-val 0)))

(defmethod .= ((arg1 tensor-scalar) (arg2 tensor-lst))
    (make-instance 'tensor-lst :init-val (dyadic-tns-scalar #'.= arg1 (get-content arg2))))

(defmethod .= ((arg1 tensor-lst) (arg2 tensor-scalar))
    (make-instance 'tensor-lst :init-val (dyadic-tns-scalar #'.= arg2 (get-content arg1))))

(defmethod .= ((arg1 tensor-lst) (arg2 tensor-lst))
    (if (not (null (dyadic-equal-dim (get-content (shape arg1)) (get-content (shape arg2)))))
        (make-instance 'tensor-lst :init-val (dyadic-tns-tns #'.= (get-content arg1) (get-content arg2)))
        nil))


;.or Same as the previous one, but using the logical disjunction.
(defgeneric .or (arg1 arg2))
(defmethod .or ((arg1 tensor-scalar) (arg2 tensor-scalar))
    (let ((result nil))
    (cond ((and (= 0 (get-content arg1)) (= 0 (get-content arg2))) (setf result 0))
        ((and (= 0 (get-content arg1)) (= 1 (get-content arg2))) (setf result 1))        
        ((and (= 1 (get-content arg1)) (= 0 (get-content arg2))) (setf result 1))
        ((and (= 1 (get-content arg1)) (= 1 (get-content arg2))) (setf result 1))
        (t nil))
    (make-instance 'tensor-scalar :init-val result)))

(defmethod .or ((arg1 tensor-scalar) (arg2 tensor-lst))
    (make-instance 'tensor-lst :init-val (dyadic-tns-scalar #'.or arg1 (get-content arg2))))

(defmethod .or ((arg1 tensor-lst) (arg2 tensor-scalar))
    (make-instance 'tensor-lst :init-val (dyadic-tns-scalar #'.or arg2 (get-content arg1))))

(defmethod .or ((arg1 tensor-lst) (arg2 tensor-lst))
    (if (not (null (dyadic-equal-dim (get-content (shape arg1)) (get-content (shape arg2)))))
        (make-instance 'tensor-lst :init-val (dyadic-tns-tns #'.or (get-content arg1) (get-content arg2)))
        nil))


;.and Same as the previous one, but using the logical conjunction. 
(defgeneric .and (arg1 arg2))
(defmethod .and ((arg1 tensor-scalar) (arg2 tensor-scalar))
    (let ((result nil))
    (cond ((and (= 0 (get-content arg1)) (= 0 (get-content arg2))) (setf result 0))
        ((and (= 0 (get-content arg1)) (= 1 (get-content arg2))) (setf result 0))        
        ((and (= 1 (get-content arg1)) (= 0 (get-content arg2))) (setf result 0))
        ((and (= 1 (get-content arg1)) (= 1 (get-content arg2))) (setf result 1))
        (t nil))
    (make-instance 'tensor-scalar :init-val result)))

(defmethod .and ((arg1 tensor-scalar) (arg2 tensor-lst))
    (make-instance 'tensor-lst :init-val (dyadic-tns-scalar #'.and arg1 (get-content arg2))))

(defmethod .and ((arg1 tensor-lst) (arg2 tensor-scalar))
    (make-instance 'tensor-lst :init-val (dyadic-tns-scalar #'.and arg2 (get-content arg1))))

(defmethod .and ((arg1 tensor-lst) (arg2 tensor-lst))
    (if (not (null (dyadic-equal-dim (get-content (shape arg1)) (get-content (shape arg2)))))
        (make-instance 'tensor-lst :init-val (dyadic-tns-tns #'.and (get-content arg1) (get-content arg2)))
        (error "TENSORS HAVE DIFFERENT DIMENSIONS")))

;(define-condition a-condition-with-no-handler (condition) ())
;DUVIDAS!!!! 
;(1) basta fazer este error???! ou é preciso ser o signal?-> como é que funciona? 
;(2) é preciso considerar outro tipo de errors. tipo se se tentar dividir por 0. ou se tentar fazer .and de nao binarios.



;tally - given a tensor, returns a scalar with the number of elements of the tensor.
(defgeneric tally (tnsr))
(defmethod tally ((tnsr tensor-scalar))
    (make-instance 'tensor-scalar :init-val 1))

(defmethod tally ((tnsr tensor-lst))
    (make-instance 'tensor-scalar :init-val (length (get-content (tensor-to-vector tnsr)))))

;ravel - given a tensor, returns a vector containing all the elements of the tensor.
(defgeneric ravel (tnsr))
(defmethod ravel ((tnsr tensor-scalar))
    (make-instance 'tensor-lst :init-val (list tnsr)))

(defmethod ravel ((tnsr tensor-lst))
    (tensor-to-vector tnsr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;ISTO é para o caso de ter de ser os inteiros. mas ainda nao está bem feito
; ;ravel - given a tensor, returns a vector containing all the elements of the tensor.
; (defgeneric ravel (tnsr))
; (defmethod ravel ((tnsr tensor-scalar))
;     (make-instance 'tensor-lst :init-val (get-content tnsr)))

; (defmethod ravel ((tnsr tensor-lst))
;     (defun ravel-aux (lst)
;         (let ((final-lst (list)))
;             (loop for x in lst
;                 do (setf final-lst (append final-lst (list (get-content x)))))
;         final-lst))
;     (make-instance 'tensor-lst :init-val (ravel-aux (get-content (tensor-to-vector tnsr)))))






