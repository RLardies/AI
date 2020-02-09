
;;Debería controlar que el divisor sea cercano a cero?
(defun newton (f df-dx max-iter x0 &optional (tol-abs 0.0001))
	(let ((xn1 (- x0 (/ (funcall f x0) (funcall df-dx x0)))))
	(cond ((< max-iter 0) NIL) ((< (abs (- xn1 x0)) tol-abs) xn1)
	(t (newton f df-dx (- max-iter 1) xn1 tol-abs)))))

(defun newton-all (f df-dx max-iter seeds &optional (tol-abs 0.0001))
	(mapcar #'(lambda (x0) (newton f df-dx max-iter x0 tol-abs)) seeds))