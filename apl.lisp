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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               GENERICS                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric get-content (object))
(defgeneric s (arg))
(defgeneric equal-tensor (tnsr1 tnsr2))
(defgeneric v-from-lst (lst))
(defgeneric PRINT-OBJECT-STRING (object))
(defgeneric tensor-to-vector (tnsr))
(defgeneric tensor-to-vector-aux (tnsr))
(defgeneric hack-dims (dims))
(defgeneric shape (tnsr))
(defgeneric interval (num))
(defgeneric fold (func))
(defgeneric scan (func))
(defgeneric outer-product (func))
(defgeneric split (lst size))
(defgeneric inner-product (func1 func2))
(defgeneric drop (n1 tensor))
(defgeneric reshape (dims fill-data))
(defgeneric catenate (arg1 arg2))
(defgeneric member? (tnsr elems))
(defgeneric select (vec tnsr))

(defgeneric .-Monadic (arg))
(defgeneric ./Monadic (arg))
(defgeneric .! (arg))
(defgeneric .sin (arg))
(defgeneric .cos (arg))
(defgeneric .not (arg))
(defgeneric .+ (arg1 arg2))
(defgeneric .-Dyadic (arg1 arg2))
(defgeneric .* (arg1 arg2))
(defgeneric ./Dyadic (arg1 arg2))
(defgeneric .// (arg1 arg2))
(defgeneric .% (arg1 arg2))
(defgeneric .< (arg1 arg2))
(defgeneric .> (arg1 arg2))
(defgeneric .<= (arg1 arg2))
(defgeneric .>= (arg1 arg2))
(defgeneric .= (arg1 arg2))
(defgeneric .or (arg1 arg2))
(defgeneric .and (arg1 arg2))

(defgeneric tally (tnsr))
(defgeneric rank (tnsr))
(defgeneric ravel (tnsr))
(defgeneric within (tnsr s1 s2))
(defgeneric primes (nmbr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod get-content ((object tensor))
    (slot-value object 'slot-content))

(defmethod s ((arg number)) (make-instance 'tensor-scalar :init-val arg))

(defun v (&rest args)
    (defun v-aux (lst)
        (if (null lst)
            '()
            (cons (s (car lst)) (v-aux (cdr lst)))))
    (make-instance 'tensor-lst :init-val (v-aux args)))



(defmethod equal-tensor ((tnsr1 t) (tnsr2 t))
    NIL)
(defmethod equal-tensor ((tnsr1 tensor-lst) (tnsr2 tensor-lst))
    (if (and (null (get-content tnsr1)) (null (get-content tnsr1)))
        T
        (and
            (equal-tensor (car (get-content tnsr1)) (car (get-content tnsr2)))
            (equal-tensor (v-from-lst (cdr (get-content tnsr1))) (v-from-lst (cdr (get-content tnsr2)))))))
(defmethod equal-tensor ((tnsr1 tensor-scalar) (tnsr2 tensor-scalar))
    (eq (get-content tnsr1) (get-content tnsr2)))


; (defgeneric hard-tensor-copy (tnsr))
; (defmethod hard-tensor-copy ((tnsr tensor-scalar))
;     (s (get-content tnsr)))

; (defmethod hard-tensor-copy ((tnsr tensor-lst))
;     (defun hard-tensor-copy-recursive (lst)
;         (if (null lst)
;             lst
;             (cons (hard-tensor-copy (car lst)) (hard-tensor-copy-recursive (cdr lst)))))
;     (let ((v-tnsr (tensor-to-vector tnsr)))
;         (reshape
;             (shape tnsr)
;             (v-from-lst (hard-tensor-copy-recursive (get-content v-tnsr))))))

; (defgeneric get-scalar-from-pos (tnsr temp-coords))
; (defmethod get-scalar-from-pos ((tnsr tensor-lst) (temp-coords tensor-lst))
;     (let ((result tnsr)
;             (coords (v-from-lst (nreverse (get-content (hack-dims temp-coords))))))
;         (dolist (dim-pos (get-content coords))
;             (setq result (nth (1- (get-content dim-pos)) (get-content result))))
;         result))

; (defgeneric set-scalar-from-pos (tnsr temp-coords scalar))
; (defmethod set-scalar-from-pos ((tnsr tensor-lst) (temp-coords tensor-lst) (scalar tensor-scalar))
;     (let ((result tnsr)
;             (coords (v-from-lst (nreverse (get-content (hack-dims temp-coords))))))
;         (dolist (dim-pos (get-content coords))
;             (setq result (nth (1- (get-content dim-pos)) (get-content result))))
;         (setf (slot-value result 'slot-content) (get-content scalar))
;         result))

(defmethod v-from-lst ((lst list))
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
    (v-from-lst (reverse (get-content dims))))

; ;queria isto mas vendo a class! é possivel?
; (defun tnsr-lst? (tnsr)
;     (if (eq (type-of tnsr) 'tensor-lst)
;         t
;         nil))

; (defun tnsr-scalar? (tnsr)
;     (if (eq (type-of tnsr) 'tensor-scalar)
;         t
;         nil))

; (defun flatten (mylist)
;   (cond
;    ((null mylist) nil)
;    ((atom mylist) (list mylist))
;    (t
;     (append (flatten (car mylist)) (flatten (cdr mylist))))))

; (defun remove-nth (n list)
;   (declare
;     (type (integer 0) n)
;     (type list list))
;   (if (or (zerop n) (null list))
;     (cdr list)
;     (cons (car list) (remove-nth (1- n) (cdr list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; CARLOS ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;shape - Creates a vector containing the length of 
;        each dimension of the argument tensor. 
(defmethod shape ((tnsr tensor-scalar))
    NIL)
(defmethod shape ((tnsr tensor-lst))
    (let ((dimension (shape (car (get-content tnsr)))))
            (if (null dimension)
                (v-from-lst (list (s (list-length (get-content tnsr)))))
                (v-from-lst (append (list (s (list-length (get-content tnsr)))) (get-content dimension))))))

;interval - Creates a vector containing an enumeration
;           of all integers starting from 1 up to the
;           argument.
(defmethod interval ((num number))
    (defun recursive-interval (num lst)
        (if (<= num 1)
            (cons (s 1) lst)
            (recursive-interval (- num 1) (cons (s num) lst))))
    (v-from-lst (recursive-interval num '())))
(defmethod interval ((num tensor-scalar))
    (interval (get-content num)))

;fold - Accepts a function and returns another function
;       that, given a vector, computes the application
;       of the function to sucessive elements of the vector.
(defmethod fold ((func function))
    (defun fold-aux (vctr)
        (let ((res NIL))
                (dolist (element (get-content vctr))
                    (if (null res)
                        (setq res element)
                        (setq res (funcall func res element))))
            res))
    #'fold-aux)

;scan - Similar to fold but using increasingly large
;       subsets of the elements of the vector, starting
;       from a subset containing just the first element
;       up to a subset containing all elements. 
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
            (reshape (v-from-lst (append shape1 shape2)) (v-from-lst result))))
    #'outer-product-aux)



(defmethod split ((lst list) (size number))
    (if (null lst)
        NIL
        (let ((small NIL))
            (dotimes (i size)
                (setq small (append small (list (nth i lst)))))
            (append (list small) (split (nthcdr size lst) size)))))


; inner-product - Accepts two functions and returns a function that, given two
;                 tensors, returns a new tensor computed according to the rules of the algebraic
;                 inner product but replacing the algebraic sum and product with
;                 the first and second functions.

(defmethod inner-product ((func1 function) (func2 function))
    (defun inner-product-aux (tns1 tns2)
        (defun recursive-inner-func (num lst2)
            (if (null lst2)
                lst2
                (list* (funcall func2 num (car lst2)) (recursive-inner-func num (cdr lst2)))))
        (let* ((tnsr1 (if (length (get-content (shape tns1))) (reshape (catenate (v 1) (shape tns1)) tns1)))
                (tnsr2 (if (length (get-content (shape tns2))) (reshape (catenate (shape tns2) (v 1)) tns2)))
                (shape1 (shape tnsr1))
                (shape2 (shape tnsr2))
                (final-shape (catenate
                    (v-from-lst (butlast (get-content shape1) 1))
                    (v-from-lst (cdr (get-content shape2)))))
                (vec1 (split (get-content (tensor-to-vector tnsr1)) (get-content (car (last (get-content shape1))))))
                (vec2-temp (get-content (tensor-to-vector tnsr2)))
                (vec2 (split vec2-temp (/ (length vec2-temp) (get-content (car (get-content shape2))))))
                (result NIL)
                (temp-result NIL))
            (dolist (vec vec1)
                (dotimes (i (length vec))
                    (if (= 0 i)
                        (setq temp-result (v-from-lst (recursive-inner-func (nth i vec) (nth i vec2))))
                        (setq temp-result (funcall func1
                                        temp-result
                                        (v-from-lst (recursive-inner-func (nth i vec) (nth i vec2)))))))
                (setq result (append result (get-content temp-result))))
            (setq result (reshape final-shape (v-from-lst result)))
            (if (equal-tensor (v 1 1) final-shape)
                (car (get-content (car (get-content result))))
                result)))
        #'inner-product-aux)

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

(defun verify-equal-or-above (n shp) ;verifica se algum valores ultrapassa ou e igual ao tamanho da dim a que se refere
	   (> (get-content (funcall (fold #'.+) (.>= n shp))) 0)) 


(defmethod drop ((n1 tensor-scalar) (tnsr tensor))
	(let ((n (get-content n1)))
		(cond 
			((null (get-content tnsr)) NIL) 
			((= n 0) tnsr)
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
	(if (verify-equal-or-above n1 (shape tnsr1))
		Nil
		(drop-aux (hack-dims n1) tnsr1)))


;reshape - Returns a tensor with the dimensions refered in the first argument,
;          whose elements are taken from the second argument, repeating them if
;          necessary to fill the resulting tensor

(defun rotate (vec)
	(let ((vec-c (get-content vec)))
  (v-from-lst (append (rest vec-c) (list (first vec-c))))))


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
    (progn (setf  fill-data-var (tensor-to-vector fill-data))
        (reshape-aux (hack-dims dims))))

(defmethod reshape ((dims tensor-scalar) (fill-data tensor-lst))
    (progn (setf  fill-data-var (tensor-to-vector fill-data))
        (reshape-aux (hack-dims (v (get-content dims))))))


;catenate - If the two arguments are scalars, returns a vector containing those
;			arguments. If the two arguments are tensors, returns a tensor that joins
;			the arguments along the their last dimension.



(defun catenate-aux (arg1-vec arg2-vec n-cols-arg1 n-cols-arg2 vec-final)
	(if (and (= 0 (list-length arg1-vec)) (= 0 (list-length arg2-vec)))
		(v-from-lst vec-final)
		(let* ((start-arg1-vec (subseq arg1-vec 0 n-cols-arg1))
			  (start-arg2-vec (subseq arg2-vec 0 n-cols-arg2))
			  (rest-arg1-vec (nthcdr n-cols-arg1 arg1-vec))
			  (rest-arg2-vec (nthcdr n-cols-arg2 arg2-vec))
			  (new-vec-final (append vec-final start-arg1-vec start-arg2-vec)))
				(catenate-aux rest-arg1-vec rest-arg2-vec n-cols-arg1 n-cols-arg2 new-vec-final))))

(defmethod catenate ((arg1 tensor-lst) (arg2 tensor-lst))	
	(let* ((arg1-vec (get-content (tensor-to-vector arg1)))
		   (arg2-vec (get-content (tensor-to-vector arg2)))
		   (n-cols-arg1 (get-content (car (last (get-content (shape arg1))))))
		   (n-cols-arg2 (get-content (car (last (get-content (shape arg2))))))
		   (final-shape-without-cols (reverse (rest (reverse (get-content (shape arg1))))))
		   (final-shape (v-from-lst (append final-shape-without-cols (list (s (+ n-cols-arg1 n-cols-arg2)))))))
		(reshape final-shape (catenate-aux arg1-vec arg2-vec n-cols-arg1 n-cols-arg2 '()))))

(defmethod catenate ((arg1 tensor-scalar) (arg2 tensor-scalar))
	(v (get-content arg1) (get-content arg2)))

(defmethod catenate ((arg1 tensor-lst) (arg2 tensor-scalar))
    (catenate arg1 (reshape (v-from-lst (append (butlast (get-content (shape arg1))) (list (s 1)))) (v (get-content arg2)))))

(defmethod catenate ((arg1 tensor-scalar) (arg2 tensor-lst))
    (catenate (reshape (v-from-lst (append (butlast (get-content (shape arg2))) (list (s 1)))) (v (get-content arg1))) arg2))

;member? - Returns a tensor of booleans with the same shape and dimension of the
; 			first argument, containing 1 for each element in the corresponding location
; 			in the first argument that occurs somewhere in the second argument and
; 			0 otherwise.



; (defun generate-all-pos-from-shape-aux (shp pos)
; 	(if (= 0 (list-length shp))
; 		(v-from-lst pos) 
; 		(let ((limit (get-content (car shp))))
; 				 	(loop for pos_i in (get-content (interval limit)) 
; 						collect (generate-all-pos-from-shape-aux (rest shp) (append pos (list pos_i)))))))

; (defun generate-all-pos-from-shape (shp)
; 	(let ((shp-ct (get-content shp)))
; 		(flatten (generate-all-pos-from-shape-aux shp-ct '()))))


 
(defmethod member? ((tnsr tensor-scalar) (elems tensor-lst)) 
	(let* ((elems-vec (get-content (tensor-to-vector elems)))
		  (rest-elems-vec (rest elems-vec)))
			(if (= (get-content tnsr) (get-content (car elems-vec)))
				(s 1)
				(if (= (list-length rest-elems-vec) 0)
					(s 0)
					(member? tnsr (v-from-lst rest-elems-vec))))))


; (defmethod member? ((tnsr tensor-lst) (elems tensor-lst))
; 	(let ((new-tnsr (hard-tensor-copy tnsr))
; 		 (shp (shape tnsr)))
; 			(dolist (pos (generate-all-pos-from-shape shp))
; 		 		(let* ((result (get-scalar-from-pos tnsr pos))
; 		 			  (is-member (member? result elems)))
; 		 			(set-scalar-from-pos new-tnsr pos is-member)))
; 				new-tnsr))

(defmethod member? ((tnsr tensor-lst) (elems tensor-lst))
    (let ((vec1 (tensor-to-vector tnsr))
            (vec2 (tensor-to-vector elems))
            (result NIL))
        (dolist (elem (get-content vec1))
            (setq result (append result (list (member? elem vec2)))))
        (reshape (shape tnsr) (v-from-lst result))))

;select - From a tensor of booleans and another tensor, returns a tensor containing
;  		  only the elements of the last dimension of the second argument whose
; 		  corresponding element in the first tensor is 1.


(defun get-pos-of-columns (lst)
    (let ((res NIL))
        (dotimes (i (length (get-content lst)))
            (if (equal-tensor (s 1) (nth i (get-content lst)))
                (setq res (append res (list i)))))
        res))

(defmethod select ((vec tensor-lst) (tnsr tensor-lst))
    (if (null (get-pos-of-columns vec))
        NIL
        (if (eq 'tensor-scalar (type-of (car (get-content tnsr))))
            (let ((res NIL))
                (dolist (pos (get-pos-of-columns vec))
                    (setq res (append res (list (nth pos (get-content tnsr))))))
                (if (null res)
                    res
                    (v-from-lst res)))
            (v-from-lst
                (list*
                    (select vec (car (get-content tnsr)))
                    (if (null (cdr (get-content tnsr)))
                        NIL
                        (get-content (select vec (v-from-lst (cdr (get-content tnsr)))))))))))



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
(defmethod .-Monadic ((arg tensor-scalar))
    (make-instance 'tensor-scalar :init-val (- 0 (get-content arg))))

(defmethod .-Monadic ((arg tensor-lst))
    (make-instance 'tensor-lst :init-val (monadic-tns #'.-Monadic (get-content arg))))


;./ Same as the previous one, but using the inverse. 
(defmethod ./Monadic ((arg tensor-scalar))
    (make-instance 'tensor-scalar :init-val (/ 1 (get-content arg))))

(defmethod ./Monadic ((arg tensor-lst))
    (make-instance 'tensor-lst :init-val (monadic-tns #'./ (get-content arg))))

;.! Same as the previous one, but using the factorial.
(defmethod .! ((arg tensor-scalar))
    (defun factorial (n)
        (if (= n 0)
            1
            (* n (factorial (- n 1)))))
    (make-instance 'tensor-scalar :init-val (factorial (get-content arg))))

(defmethod .! ((arg tensor-lst))
    (make-instance 'tensor-lst :init-val (monadic-tns #'.! (get-content arg))))

;.sin Same as the previous one, but using the sin function. 
(defmethod .sin ((arg tensor-scalar))
    (make-instance 'tensor-scalar :init-val (sin (get-content arg))))

(defmethod .sin ((arg tensor-lst))
    (make-instance 'tensor-lst :init-val (monadic-tns #'.sin (get-content arg))))


;.cos Same as the previous one, but using the cos function. 
(defmethod .cos ((arg tensor-scalar))
    (make-instance 'tensor-scalar :init-val (cos (get-content arg))))

(defmethod .cos ((arg tensor-lst))
    (make-instance 'tensor-lst :init-val (monadic-tns #'.cos (get-content arg))))


;.not Same as the previous one, but using the negation. 
;The result is a tensor containing, as elements, the integers 
;0 or 1, depending on the corresponding element in the 
;argument tensor being different that zero or equal to zero.
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
(defmethod tally ((tnsr tensor-scalar))
	(s 1))

(defmethod tally ((tnsr tensor-lst))
    (funcall (fold #'.*) (shape tnsr)))


;rank -	Define the function rank that, given a tensor, returns a scalar with the
;		number of dimensions of the tensor.
(defmethod rank ((tnsr tensor-scalar))
	(s 1))

(defmethod rank ((tnsr tensor-lst))
	(funcall (fold #'.+) (.not (.= (reshape (shape (shape tnsr)) (v 0)) (shape tnsr)))))



;ravel - given a tensor, returns a vector containing all the elements of the tensor.
(defmethod ravel ((tnsr tensor-scalar))
    (make-instance 'tensor-lst :init-val (list tnsr)))

(defmethod ravel ((tnsr tensor-lst))
    (reshape (tally tnsr) tnsr))

; within - given a vector of numbers v and two
;          numbers n1 and n2, returns a vector containing only the elements of v
;          that are in the range between n1 and n2.
(defmethod within ((tnsr tensor-lst) (s1 tensor-scalar) (s2 tensor-scalar))
    (let ((x (drop (.- s1 (s 1)) (interval s2)))
          (y (reshape (shape tnsr) (v 0))))
        (select (.not (.= (.* (member? tnsr x) tnsr) y)) tnsr)))

; primes - given a scalar, returns a vector with all
;          prime numbers from 2 up to the scalar, inclusive.
(defmethod primes ((nmbr tensor-scalar))
    (let ((R (drop (v 1) (interval nmbr))))
        (select (.not (member? R (funcall (outer-product #'.*) R R))) R)))
