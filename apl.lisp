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

(defun s (arg) `#(,arg))

(defun v (&rest args) args (make-array (list-length args) :initial-contents args))


(defun print-tensor (tensor) tensor)