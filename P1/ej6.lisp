
(defun angular-distance (x y) 
	(/ (acos (cosine-similarity x y)) pi))


(defun nearest-neighbor (lst-vectors test-vector distance-fn)
	(if (null lst-vectors)
		nil
		(if (null (rest lst-vectors))
			(list (first lst-vectors) (funcall distance-fn (first lst-vectors) test-vector))
			(if (< (funcall distance-fn (first lst-vectors) test-vector) (funcall distance-fn (second lst-vectors) test-vector))
				(nearest-neighbor (remove (second lst-vectors) lst-vectors) test-vector distance-fn)
				(nearest-neighbor (remove (first lst-vectors) lst-vectors) test-vector distance-fn)))))