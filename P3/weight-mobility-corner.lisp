(defvar *alias* '|Empezamos a ponernos serios|)

;; Función de evalueación heurística
(defun eval-fn (player board)
  (let ((status (game-status player board))
        (mobility-value (mobility player board))
        (weight-value (weight-fn player board))
        (weight2-value (weight2-fn player board))
        (opponent-legal-moves (legal-moves (opponent player) board)))

    (if (parity-check player board)
      10000
      (if (corner-ocuped-check player board)
        (+ (/ (* status weight2-value) 64) (/ (* (- 64 status) mobility-value) 64))
        (+ (/ (* status weight-value) 64) (/ (* (- 64 status) mobility-value) 64))))))

;; Funciones auxiliares 

(defparameter *pesos* 
  '((100 -20 10 5 5 10 -20 100)
    (-20 -50 -2 -2 -2 -2 -50 -20)
    (10 -2 -1 -1 -1 -1 -2 10)
    (5 -2 -1 -1 -1 -1 -2 5)
    (5 -2 -1 -1 -1 -1 -2 5)
    (10 -2 -1 -1 -1 -1 -2 10)
    (-20 -50 -2 -2 -2 -2 -50 -20)
    (100 -20 10 5 5 10 -20 100)))

(defparameter *pesos2* 
  '((100 40 10 5 5 10 40 100)
    (40 50 -2 -2 -2 -2 50 40)
    (10 -2 -1 -1 -1 -1 -2 10)
    (5 -2 -1 -1 -1 -1 -2 5)
    (5 -2 -1 -1 -1 -1 -2 5)
    (10 -2 -1 -1 -1 -1 -2 10)
    (40 50 -2 -2 -2 -2 50 40)
    (100 40 10 5 5 10 40 100)))

(defun weight-fn (player board)
  (let ((brd (get-board board)))
    (apply #'+ 
      (mapcar 
        #'(lambda (boardrow weightrow) (row-value player boardrow weightrow))
        brd *pesos*))))

(defun weight2-fn (player board)
  (let ((brd (get-board board)))
    (apply #'+ 
      (mapcar 
        #'(lambda (boardrow weightrow) (row-value player boardrow weightrow))
        brd *pesos2*))))

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

(defun parity-check (player board)
  (let ((status (game-status player board)))
    (and (null (legal-moves (opponent player) board))
         (or (and (= player 1) (oddp status))
             (and (= player 2) (evenp status))))))

(defun mobility (player board)
  "The number of moves a player has."
  (length (legal-moves player board)))
   weight3-mobility

(defun corner-ocuped-check (player board)
  (let ((brd (get-board board)))
    (if (and (and (or (= (first (first brd)) 2) (= (first (first brd)) 1)) 
                  (or (= (first (octavo brd)) 2) (= (first (octavo brd)) 1)))
             (and (or (= (octavo (first brd)) 2) (= (octavo (first brd)) 1)) 
                  (or (= (octavo (octavo brd)) 2) (= (octavo (octavo brd)) 1))))
      t)))

(defun octavo(L)
  (second (cdr (cdr (cdr (cdr (cdr (cdr L))))))))