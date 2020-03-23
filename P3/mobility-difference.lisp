;; Alias que aparece en el ranking

(defvar *alias* '|A ver quién se mueve mejor|)

;; Función de evalueación heurística

(defun eval-fn (player board)
	(- (legal-moves player board) (legal-moves (opponent player) board)))