(defun scalar-product (x y)
	(apply #'+ (mapcar #'* x y)))

;;Aquí se podría hacer con la anterior pero croe uqe así es más eficiente, preguntar
(defun euclidean-norm (x)
	(sqrt (apply #'+ (mapcar #'(lambda (x0) (* x0 x0)) x))))

(defun euclidean-distance (x y)
	(euclidean-norm (mapcar #'- x y)))