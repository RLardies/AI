(defun backward-chaining (goal lst-rules) 
	(backward-chaining-aux goal lst-rules NIL))

(defun backward-chaining-aux (goal lst-rules pending-goals)
	(if (member goal pending-goals) NIL
		(some #'(lambda (x) (if (equal (second x) goal) (if (null (car x)) t
			(every #'identity (mapcar #'(lambda (y) 
				(backward-chaining-aux y (remove x lst-rules) (cons goal pending-goals))) (car x))))))
		lst-rules)))