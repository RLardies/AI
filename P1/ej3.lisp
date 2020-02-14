(defun scalar-product (x y)
	(apply #'+ (mapcar #'* x y)))

;;Aquí se podría hacer con la anterior pero creo uqe así es más eficiente, preguntar
(defun euclidean-norm (x)
	(sqrt (apply #'+ (mapcar #'(lambda (x0) (* x0 x0)) x))))

(defun euclidean-distance (x y)
	(euclidean-norm (mapcar #'- x y)))



(equal (scalar-product '(1 2 3 4) '(1 1 2 1)) 13) ;; Test simple
(equal (scalar-product '(1 2 3) '(0 0 0)) 0);; Test Simple


(equal (euclidean-norm '(-1 2 -5)) (sqrt 30)) ;; Test Simple
(equal (euclidean-norm '(0 0 0)) (sqrt 0)) ;; Test Simple


(equal (euclidean-distance '(1 2 3) '(2 4 -8)) (sqrt 126)) ;; Test Simple 
(equal (euclidean-distance '(1 1 1) '(1 1 1)) 0) ;; Test Simple
(equal (euclidean-distance '(1 1 1) '(1 2 3)) (sqrt 5)) ;; Test Simple
