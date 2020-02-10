(defun backward-chaining (goal lst-rules) 
	(backward-chaining-aux goal lst-rules NIL))

(defun backward-chaining-aux (goal lst-rules pending-goals)
	(if (member goal pending-goals) NIL 
		(some #'(lambda (x) (if (equal (second x) goal) (if (null (car x)) t
			(and (backward-chaining-aux (car (car x)) (remove x lst-rules) (cons goal pending-goals))
				(backward-chaining-aux (second (car x)) (remove x lst-rules) (cons goal pending-goals)))))) lst-rules)))