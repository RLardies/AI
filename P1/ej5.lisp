(defun select-vectors (lst-vectors test-vector similarity-fn &optional (threshold 0))
	(sort (remove-if #'(lambda (x) (< (second x) threshold))
		(mapcar #'(lambda (lst) 
			(list lst (funcall similarity-fn lst test-vector))) lst-vectors)) #'(lambda (x y)
			 (> (second x) (second y)))))


