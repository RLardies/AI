(defun scalar-product (x y)
	(apply #'+ (mapcar #'* x y)))

;;Aquí se podría hacer con la anterior pero creo uqe así es más eficiente, preguntar
(defun euclidean-norm (x)
	(sqrt (apply #'+ (mapcar #'(lambda (x0) (* x0 x0)) x))))

(defun euclidean-distance (x y)
	(euclidean-norm (mapcar #'- x y)))



(equal (scalar-product '(1 2 3 4) '(1 1 1 1)) 10) ;; Test simple
(equal (scalar-product '(1 2 3) '(3 2 1)) 10);; Test Simple
(equal (euclidean-norm '(1 2 3)) (sqrt 14)) ;; Test Simple
(equal (euclidean-norm '(-1 2 -5)) (sqrt 30)) ;; Test Simple
(equal (euclidean-distance '(1 2 3) '(2 4 -8)) ) ;; Test Simple 
(equal (euclidean-distance '(1 1 2 -1) '(-1 3 -1 1)) ) ;; Test Simple
