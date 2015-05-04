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