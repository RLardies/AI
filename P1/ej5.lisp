(defun select-vectors (lst-vectors test-vector similarity-fn &optional (threshold 0))
	(sort (remove-if #'(lambda (x) (< (second x) threshold))
		(mapcar #'(lambda (lst) 
			(list lst (funcall similarity-fn lst test-vector))) lst-vectors)) #'(lambda (x y)
			 (> (second x) (second y)))))


(equal (select-vectors '((-1 -1 -1) (-1 -1 1) (-1 1 1) (1 1 1))
 '(1 1 1) #'cosine-similarity 0.2) (list (list '(1 1 1) '1.0) (list '(-1 1 1) '0.33333334))


(select-vectors '((-1 -1 -1) (-1 -1 1) (-1 1 1) (1 1 1))
 '(0 0 0) #'cosine-similarity 0.2)