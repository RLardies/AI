(defun backward-chaining (goal lst-rules) 
	(backward-chaining-aux goal lst-rules NIL))

(defun backward-chaining-aux (goal lst-rules pending-goals)
	(if (member goal pending-goals) NIL
		(some #'(lambda (x) (if (equal (second x) goal) (if (null (car x)) t
			(every #'(lambda (y) 
				(backward-chaining-aux y (remove x lst-rules) (cons goal pending-goals))) (car x)))))
		lst-rules)))

(backward-chaining 'Q '((NIL A) (NIL B) ((P) Q) ((L M) P) ((B L) M) ((A P) L) ((A B) L))) ;; Test que si encuentra
(not (backward-chaining 1 '((NIL 3) (NIL 5) ((2 3) 4) ((5 6) 2) ((3 7) 6) (NIL 8)))) ;; Test que no encuentra
(not (backward-chaining 'Q NIL)) ;; Test con nil como lista