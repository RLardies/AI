(defun combine-elt-lst (el lst)
	(mapcar #'(lambda (x) (list el  x)) lst))

(defun combine-lst-lst (lst1 lst2) 
	(mapcan #'(lambda (x) (combine-elt-lst x lst2)) lst1))

(defun combine-list-of-lsts (lstolsts) 
	(if (null (rest lstolsts)) ;Si la lista tiene un elemento
		(first lstolsts)
		(if (null (cddr lstolsts)) ; si la lista tiene dos elementos
			(combine-lst-lst (first lstolsts) (cadr lstolsts))
			(combine-lst-lst-aux (first lstolsts) (combine-list-of-lsts(rest lstolsts))))))

(defun combine-lst-lst-aux (lst1 lst2)
	(mapcan #'(lambda (x) (combine-elt-lst-aux x lst2)) lst1))

(defun combine-elt-lst-aux (el lst)
	(mapcar #'(lambda (x) (cons el  x)) lst))

;; Tests

;; Apartado 1
(equal (combine-elt-lst 1 '(1 2 3 4)) '((1 1) (1 2) (1 3) (1 4))) ;; Test simple
(equal (combine-elt-lst 1 NIL) NIL) ;; Test con lista vacía
(equal (combine-elt-lst '(1 2) '(5 6 7)) 
	'(((1 2) 5) ((1 2) 6) ((1 2) 7))) ;; Test con lista como elemento
(equal (combine-elt-lst NIL '(2 3 4)) '((NIL 2) (NIL 3) (NIL 4))) ;; Test con NIL como elemento

;; Apartado 2
(equal (combine-lst-lst '(1 2) '(a b c))
	'((1 a) (1 b) (1 c) (2 a) (2 b) (2 c)));; Test simple
(equal (combine-lst-lst '(1 2) NIL) '((1) (2))) ;; Test segunda lista vacía
(equal (combine-lst-lst NIL '(a b c)) '((a) (b) (c))) ;; Test primera lista vacía
(equal (combine-lst-lst NIL NIL) NIL) ;; Test ambas listas vacías

;; Apartado 3
 (equal (combine-list-of-lsts '((1 2) (3 4) (5 6)))
'((1 3 5) (1 3 6) (1 4 5) (1 4 6) (2 3 5) (2 3 6) (2 4 5) (2 4 6)))

(equal (combine-list-of-lsts '((1 2) (a b) ("hola" "cosa")))
	'((1 a "hola") (1 a "cosa") (1 b "hola") (1 b "cosa") 
	(2 a "hola") (2 a "cosa") (2 b "hola") (2 b "cosa"))) ;; Test simple
(equal (combine-list-of-lsts '(NIL '(1 2) '(a b))) 
	(combine-lst-lst '(1 2) '(a b))) ;; Test con primer argumento NIL
(equal (combine-list-of-lsts '(NIL '(1 2) NIL)) '(1 2)) ;; Test varios argumentos NIL
(equal (combine-list-of-lsts '(NIL NIL NIL NIL)) NIL) ;; Test con todos argumentos NIL

