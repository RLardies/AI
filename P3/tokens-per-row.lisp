;; Alias que aparece en el ranking

(defvar *alias* '|Para hacer bingo hace falta hacer fila!|)

;; Función de evalueación heurística

(defun eval-fn (player board)
	(count-tokens-per-row player board 0))

;; Funciones auxiliares 

(defun count-tokens-per-row (player board maxim)
	(if (null board)
		maxim
		(count-tokens-per-row player (cdr board) (max (count player (car board)) maxim))))
	