(load "reversi-package")
(use-package 'reversi-package)

;;(reversi #'random-strategy #'random-strategy)

;; (reversi #'random-strategy #'human)

(defun mobility (player board)
  "The number of moves a player has."
  (length (legal-moves player board)))

;; (reversi #'human (alpha-beta-searcher 2 #'count-difference))

(defun count-difference (player board)
  "Count player's pieces minus opponent's pieces."
  (let ((brd (get-board board)))
    (- (reduce #'+ (mapcar #'(lambda (row) (count player row)) brd))
       (reduce #'+ (mapcar #'(lambda (row) (count (opponent player) row)) brd)))))

(defun aleat-count-diff-mob (player board)
  (eval (elt '((mobility player board) (count-difference player board)) (random 2))))

(defun aleat-examples (player board)
  (let ((num (random 1.0)))
    (cond 
      ((< num 0.385)
        (count-difference player board))
      ((< num 0.827)
        (mobility player board))
      (t 
        (random 64)))))

(defun tokens-per-row (player board)
  (let ((brd (get-board board)))
  (apply #'max (mapcar #'(lambda (row) (count player row)) brd))))

(defun weight-fn (player board)
  (let ((brd (get-board board)))
    (apply #'+ 
      (mapcar 
        #'(lambda (boardrow weightrow) (row-value player boardrow weightrow))
        brd *pesos*))))

(defun row-value (player boardrow weightrow)
  (apply #'+ 
    (mapcar 
      #'(lambda (brd-player square-weight) (square-value player brd-player square-weight))
      boardrow weightrow)))

(defun square-value (player brd-player square-weight)
  (cond 
    ((= brd-player player)
      square-weight)
    ((= brd-player (opponent player))
      (- square-weight))
    (t
      0)))

(defun game-status (player board)
  (let ((brd (get-board board)))
    (apply #'+ (mapcar #'(lambda (row)
      (apply #'+ (mapcar #'(lambda (square)
        (if (or (= square player) (= square (opponent player))) 1 0)) row))) brd))))

(defun weight-mobility (player board)
  (let ((status (game-status player board))
        (mobility-value (mobility player board))
        (weight-value (weight-fn player board)))
    (+ (/ (* status weight-value) 64) (/ (* (- 64 status) mobility-value) 64))))

;; (reversi #'human (alpha-beta-searcher 2 #'mobility))

(round-robin
 (list (alpha-beta-searcher 2 #'count-difference)
       (alpha-beta-searcher 2 #'mobility)
       (alpha-beta-searcher 2 #'weight-fn)
       (alpha-beta-searcher 2 #'weight-mobility)
       #'random-strategy)
 20
 10
 '(count-difference
   mobility
   weight-fn
   weight-mobility
   random-strategy))

(defparameter *pesos* 
  '((100 -20 10 5 5 10 -20 100)
    (-20 -50 -2 -2 -2 -2 -50 -20)
    (10 -2 -1 -1 -1 -1 -2 10)
    (5 -2 -1 -1 -1 -1 -2 5)
    (5 -2 -1 -1 -1 -1 -2 5)
    (10 -2 -1 -1 -1 -1 -2 10)
    (-20 -50 -2 -2 -2 -2 -50 -20)
    (100 -20 10 5 5 10 -20 100)))