
(defun angular-distance (x y) 
	(/ (acos (cosine-similarity x y)) pi))


(defun nearest-neighbor (lst-vectors test-vector distance-fn)

	(first (sort (mapcar #'(lambda (lst) 
		(list lst (funcall distance-fn lst test-vector))) lst-vectors) #'(lambda (x y)
			(< (second x) (second y))))))