(defun cosine-similarity (x y)
	(let ((scal-product (scalar-product x y)) 
		(norm-product (* (euclidean-norm x) (euclidean-norm y))))
		(if (/= 0 norm-product) ;; Si el producto de las normas es 0, devolver NIL
			(/ scal-product norm-product ))))

(defun angular-distance (x y) 
	(let ((cos-sim (cosine-similarity x y)))
    	(if cos-sim
      		(/ (acos cos-sim) pi))))

;; Tests

;; Apartado 1
(< (abs (- (cosine-similarity '(1 2 3) '(1 2 3)) 1)) 1.0L-6) ;; Test vectores iguales
(< (abs (- (cosine-similarity '(1 2 3) '(-4.5 7 0)) 0.3051052626)) 1.0L-6) ;; Test simple
(null (cosine-similarity '(0 0 0) '(1 2 3))) ;; Test con vector 0

;; Apartado 2
(< (abs (- (angular-distance '(1 2 3) '(1 2 3)) 0)) 1.0L-3) ;; Test vectores iguales
(< (abs (- (angular-distance '(1 2 3) '(-4.5 7 0)) 0.4013083507)) 1.0L-3) ;; Test simple
(null (angular-distance '(0 0 0) '(1 2 3))) ;; Test con vector 0