(defun nearest-neighbor (lst-vectors test-vector distance-fn)
	(if (and lst-vectors test-vector)
		(if (null (rest lst-vectors))
			(list (first lst-vectors) (funcall distance-fn (first lst-vectors) test-vector))
			(if (< (funcall distance-fn (first lst-vectors) test-vector) (funcall distance-fn (second lst-vectors) test-vector))
				(nearest-neighbor (remove (second lst-vectors) lst-vectors) test-vector distance-fn)
				(nearest-neighbor (remove (first lst-vectors) lst-vectors) test-vector distance-fn)))))

;; Tests

(equal (nearest-neighbor '((1 2 3) (-1 2 5) (-2 -3 1)) '(1 1 1) #'angular-distance) '((1 2 3) 0.12337583)) ;; Test simple
(null (nearest-neighbor NIL '(1 2 3) #'angular-distance)) ;; Lista de vectores vacía
(null (nearest-neighbor '((1 2 3) (4 5 6)) NIL #'angular-distance)) ;; Vector test NIL