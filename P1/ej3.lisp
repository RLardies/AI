(defun scalar-product (x y)
	(apply #'+ (mapcar #'* x y)))

(defun euclidean-norm (x)
	(sqrt (apply #'+ (mapcar #'(lambda (x0) (* x0 x0)) x))))

(defun euclidean-distance (x y)
	(euclidean-norm (mapcar #'- x y)))

;;Tests

;; Apartado 1
(= (scalar-product '(1 2 3 4) '(1 1 2 1)) 13) ;; Test simple
(= (scalar-product '(1 2 3) '(0 0 0)) 0);; Test con vector 0

;; Apartado 2
(= (euclidean-norm '(-1 2 -5)) (sqrt 30)) ;; Test Simple
(= (euclidean-norm '(0 0 0)) (sqrt 0)) ;; Test con vector 0

;; Apartado 3
(= (euclidean-distance '(1 2 3) '(2 4 -8)) (sqrt 126)) ;; Test Simple 
(= (euclidean-distance '(1 1 1) '(1 1 1)) 0) ;; Test con vectores iguales
(= (euclidean-distance '(1 1 1) '(0 0 0)) (sqrt 3)) ;; Test con vector 0
