;; Alias que aparece en el ranking

(defvar *alias* '|Para bingo hace falta hacer fila|)

;; Función de evalueación heurística

(defun eval-fn (player board)
  (let ((brd (get-board board)))
  	(apply #'max (mapcar #'(lambda (row) (count player row)) brd))))
	