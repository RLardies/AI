(defun newton (f df-dx max-iter x0 &optional (tol-abs 0.0001))
	(let ((df-dx-x0 (funcall df-dx x0))) ;; Calculamos el valor de la derivada en el punto
    (if (> (abs df-dx-x0) 1.0L-15) ;; Si la derivada es 0, devolver NIL
      (let* ((f-x0 (funcall f x0)) (xn1 (- x0 (/ f-x0 df-dx-x0))))
        (cond ((< max-iter 0) NIL) ((< (abs (- xn1 x0)) tol-abs) xn1)
          (t (newton f df-dx (- max-iter 1) xn1 tol-abs)))))))

(defun newton-all (f df-dx max-iter seeds &optional (tol-abs 0.0001))
	(mapcar #'(lambda (x0) (newton f df-dx max-iter x0 tol-abs)) seeds))


(< (abs (- (newton #'sin #'cos 50 2.0) pi)) 1.0L-6)
(< (abs (- (newton #'(lambda (x) (* x x)) #'(lambda(x) (* 2 x)) 50 1.0) 0)) 1.0L-4)

(equal (newton-all #'sin #'cos 50 (mapcar #'eval '((/ pi 2) 1.0 2.0 4.0 6.0))) '(NIL 0.0 3.1415927 3.1415927 6.2831855))  

(equal (newton-all #'sin #'cos 50 '() '1) nil) 

(equal (newton-all #'sin #'cos 50 (mapcar #'eval '((/ pi 2) 1.0 2.0 4.0 6.0)) 1.0) '(NIL 0.0 3.1415927 3.1415927 6.2831855))  