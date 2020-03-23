;; Alias que aparece en el ranking

(defvar *alias* '|Aleatorio, pero no mucho|)

;; Función de evalueación heurística

(defun eval-fn (player board)
	(eval (elt '((mobility player board) (count-difference player board)) (random 2))))

;; Funciones auxiliares 

(defun mobility (player board)
  "The number of moves a player has."
  (length (legal-moves player board)))

(defun count-difference (player board)
  "Count player's pieces minus opponent's pieces."
  (let ((brd (get-board board)))
    (- (reduce #'+ (mapcar #'(lambda (row) (count player row)) brd))
       (reduce #'+ (mapcar #'(lambda (row) (count (opponent player) row)) brd)))))