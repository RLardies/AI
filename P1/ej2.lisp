(defun combine-elt-lst (el lst)
	(mapcar #'(lambda (x) (list el  x)) lst))