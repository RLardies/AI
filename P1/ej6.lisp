(defun nearest-neighbor (lst-vectors test-vector distance-fn)
	(if lst-vectors
		(if (null (rest lst-vectors))
			(list (first lst-vectors) (funcall distance-fn (first lst-vectors) test-vector))
			(if (< (funcall distance-fn (first lst-vectors) test-vector) (funcall distance-fn (second lst-vectors) test-vector))
				(nearest-neighbor (remove (second lst-vectors) lst-vectors) test-vector distance-fn)
				(nearest-neighbor (remove (first lst-vectors) lst-vectors) test-vector distance-fn)))))