
(defun cosine-similarity (x y) ;; Preguntar si debería comprobar los tamaños
	(let ((scal-product (scalar-product x y)) 
		(norm-product (* (euclidean-norm x) (euclidean-norm y))))
		(if (/= 0 norm-product) ;"Si el producto de las normas es 0, devolver NIL"
			(/ scal-product norm-product ))))

(defun angular-distance (x y) 
	(/ (acos (cosine-similarity x y)) pi))

;; Tests

;; Apartado 1
(let (( res (cosine-similarity '(1 2 3) '(1 2 3))))
	(and (> res (- 1 0.00001)) (< res (+ 1 0.00001)))) ;; Test vectores iguales
(let (( res (cosine-similarity '(1 2 3) '(-4.5 7 0))))
	(and (> res (- 1 0.00001)) (< res (+ 1 0.00001)))) ;; Test simple
;;(let (( res (cosine-similarity '(0 0 0) '(1 2 3))))
	;;(and (> res (- 1 0.00001)) (< res (+ 1 0.00001)))) No deja asignar NIL a la variable res

;; Apartado 2
(let (( res (angular-distance '(1 2 3) '(1 2 3))))
	(and (> res (- 1 0.00001)) (< res (+ 1 0.00001)))) ;; Test vectores iguales
(let (( res (angular-distance '(1 2 3) '(-4.5 7 0))))
	(and (> res (- 1 0.00001)) (< res (+ 1 0.00001)))) ;; Test simple
(let (( res (angular-distance '(0 0 0) '(1 2 3))))
	(and (> res (- 1 0.00001)) (< res (+ 1 0.00001)))) ;; Test con vector 0