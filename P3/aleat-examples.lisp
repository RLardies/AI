;; Alias que aparece en el ranking

(defvar *alias* '|Aleatorio estudiando funciones de ejemplo|)

;; Función de evalueación heurística

(defun eval-fn (player board)
	(let ((num (random 1.0)))
		(cond 
			((num < 0.385)
				(count-difference player board))
			((num < 0.827)
				(mobility player board))
			(t 
				(random 64)))))

;; Funciones auxiliares 

(defun mobility (player board)
  "The number of moves a player has."
  (length (legal-moves player board)))

(defun count-difference (player board)
  "Count player's pieces minus opponent's pieces."
  (let ((brd (get-board board)))
    (- (reduce #'+ (mapcar #'(lambda (row) (count player row)) brd))
       (reduce #'+ (mapcar #'(lambda (row) (count (opponent player) row)) brd)))))