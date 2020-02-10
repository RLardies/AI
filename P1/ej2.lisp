(defun combine-elt-lst (el lst)
	(mapcar #'(lambda (x) (list el  x)) lst))

(defun combine-lst-lst (lst1 lst2) 
	(mapcar #'(lambda(x) (combine-elt-lst x lst2)) lst1))

(defun combine-list-of-lsts (lstolsts) 

;; Tests

(equal (combine-elt-lst 1 '(1 2 3 4)) '((1 1) (1 2) (1 3) (1 4))) ;; Test simple
(equal (combine-elt-lst 1 NIL) NIL) ;; Test con lista vacía
(equal (combine-elt-lst '(1 2) '(5 6 7)) '(((1 2) 5) ((1 2) 6) ((1 2) 7))) ;; Test con lista como elemento
(equal (combine-elt-lst NIL '(2 3 4)) '((NIL 2) (NIL 3) (NIL 4))) ;; Test con NIL como elemento