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
  (let ((brd board));(get-board board)))
    (apply #'+ (mapcar #'(lambda (boardrow weightrow) 
        (apply #'+ (mapcar #'(lambda (brd-player square-weight)
          (cond 
            ((= player brd-player)
              square-weight)
            ((= player (opponent player))
              (- square-weight))
            (t
              0)))
          boardrow weightrow)))
    brd *pesos*))))

;; (reversi #'human (alpha-beta-searcher 2 #'mobility))

(round-robin
 (list (alpha-beta-searcher 2 #'count-difference)
       (alpha-beta-searcher 2 #'mobility)
       (alpha-beta-searcher 2 #'weight-fn)
       #'random-strategy)
 20
 10
 '(count-difference
   mobility
   weight-fn
   random-strategy))

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

(defparameter *boardtest*
  (list '(1 0 2)
    '(0 1 0)
    '(2 0 1)))

(defparameter *pesostest*
  (list (2 3 -2)
    (1 -2 5)
    (0 3 -5)))