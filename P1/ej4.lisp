

(defun cosine-similarity (x y) 
	(/ (scalar-product x y) (* (euclidean-norm x) (euclidean-norm y))))

(defun angular-distance (x y) 
	(/ (acos (cosine-similarity x y)) pi))

