(defun newton (f df-dx max-iter x0 &optional (tol-abs 0.0001))
	(let ((xn1 (- x0 (/ (funcall f x0) (funcall df-dx x0)))))
	(if (< (abs (- xn1 x0)) tol-abs) xn1
	(newton f df-dx max-iter xn1 tol-abs))))