(defvar *alias* '|Cansao de la cuarentena ya|)

;; Función de evalueación heurística
(defun eval-fn (player board)
  (let ((status (game-status player board))
        (mobility-value (mobility player board))
        (weight-value (weightini-fn player board))
        (weight2-value (weight2-fn player board))
        (weight3-value (weight3-fn player board))
        (weight4-value (weight4-fn player board))
        (weight5-value (weight5-fn player board))
        (weight6-value (weight6-fn player board))
        (weight7-value (weight7-fn player board))
        (weight8-value (weight8-fn player board))
        (weight9-value (weight9-fn player board))
        (weight10-value (weight10-fn player board))
        (weight11-value (weight11-fn player board))
        (weight12-value (weight12-fn player board))
        (weight13-value (weight13-fn player board))
        (weight14-value (weight14-fn player board))
        (weight15-value (weight15-fn player board))
        (weight16-value (weight16-fn player board))
        (opponent-legal-moves (legal-moves (opponent player) board)))

    (if (parity-check player board)
      10000
      (if (corner-ocuped-check player board)
        (+ (/ (* status weight16-value) 64) (/ (* (- 64 status) mobility-value) 64))
        (if (and (corner-UR-check player board) (and (corner-UL-check player board) (corner-DL-check player board)))
          (+ (/ (* status weight15-value) 64) (/ (* (- 64 status) mobility-value) 64))
          (if (and (corner-UR-check player board) (and (corner-UL-check player board) (corner-DR-check player board)))
            (+ (/ (* status weight14-value) 64) (/ (* (- 64 status) mobility-value) 64))
            (if (and (corner-UL-check player board) (and (corner-DL-check player board) (corner-DR-check player board)))
              (+ (/ (* status weight13-value) 64) (/ (* (- 64 status) mobility-value) 64))
              (if (and (corner-UR-check player board) (and (corner-DL-check player board) (corner-DR-check player board)))
                (+ (/ (* status weight12-value) 64) (/ (* (- 64 status) mobility-value) 64))
                (if (and (corner-UL-check player board) (corner-UR-check player board))
                  (+ (/ (* status weight6-value) 64) (/ (* (- 64 status) mobility-value) 64))
                  (if (and (corner-UL-check player board) (corner-DL-check player board))
                    (+ (/ (* status weight7-value) 64) (/ (* (- 64 status) mobility-value) 64))
                    (if (and (corner-UL-check player board) (corner-DR-check player board))
                      (+ (/ (* status weight8-value) 64) (/ (* (- 64 status) mobility-value) 64))
                      (if (and (corner-UR-check player board) (corner-DL-check player board))
                        (+ (/ (* status weight9-value) 64) (/ (* (- 64 status) mobility-value) 64))
                        (if (and (corner-UR-check player board) (corner-DR-check player board))
                          (+ (/ (* status weight10-value) 64) (/ (* (- 64 status) mobility-value) 64))
                          (if (and (corner-DR-check player board) (corner-DL-check player board))
                            (+ (/ (* status weight11-value) 64) (/ (* (- 64 status) mobility-value) 64))
                            (if (corner-UL-check player board)
                              (+ (/ (* status weight2-value) 64) (/ (* (- 64 status) mobility-value) 64))
                              (if (corner-UR-check player board)
                                (+ (/ (* status weight3-value) 64) (/ (* (- 64 status) mobility-value) 64))
                                (if (corner-DL-check player board)
                                  (+ (/ (* status weight4-value) 64) (/ (* (- 64 status) mobility-value) 64))
                                  (if (corner-DR-check player board)
                                    (+ (/ (* status weight5-value) 64) (/ (* (- 64 status) mobility-value) 64))
                                    (+ (/ (* status weight-value) 64) (/ (* (- 64 status) mobility-value) 64))))))))))))))))))))

;; Funciones auxiliares 

(defparameter *pesosini* 
  '((100 -20 10 5 5 10 -20 100)
    (-20 -50 -2 -2 -2 -2 -50 -20)
    (10 -2 -1 -1 -1 -1 -2 10)
    (5 -2 -1 -1 -1 -1 -2 5)
    (5 -2 -1 -1 -1 -1 -2 5)
    (10 -2 -1 -1 -1 -1 -2 10)
    (-20 -50 -2 -2 -2 -2 -50 -20)
    (100 -20 10 5 5 10 -20 100)))
;11
(defparameter *pesos2* 
  '((100 40 10 5 5 10 -20 100)
    (40 50 -2 -2 -2 -2 -50 -20)
    (10 -2 -1 -1 -1 -1 -2 10)
    (5 -2 -1 -1 -1 -1 -2 5)
    (5 -2 -1 -1 -1 -1 -2 5)
    (10 -2 -1 -1 -1 -1 -2 10)
    (-20 -50 -2 -2 -2 -2 -50 -20)
    (100 -20 10 5 5 10 -20 100)))
;18
(defparameter *pesos3* 
  '((100 -20 10 5 5 10 40 100)
    (-20 -50 -2 -2 -2 -2 50 40)
    (10 -2 -1 -1 -1 -1 -2 10)
    (5 -2 -1 -1 -1 -1 -2 5)
    (5 -2 -1 -1 -1 -1 -2 5)
    (10 -2 -1 -1 -1 -1 -2 10)
    (-20 -50 -2 -2 -2 -2 -50 -20)
    (100 -20 10 5 5 10 -20 100)))
;81
(defparameter *pesos4* 
  '((100 -20 10 5 5 10 -20 100)
    (-20 -50 -2 -2 -2 -2 -50 -20)
    (10 -2 -1 -1 -1 -1 -2 10)
    (5 -2 -1 -1 -1 -1 -2 5)
    (5 -2 -1 -1 -1 -1 -2 5)
    (10 -2 -1 -1 -1 -1 -2 10)
    (40 50 -2 -2 -2 -2 -50 -20)
    (100 40 10 5 5 10 -20 100)))
;88
(defparameter *pesos5* 
  '((100 -20 10 5 5 10 -20 100)
    (-20 -50 -2 -2 -2 -2 -50 -20)
    (10 -2 -1 -1 -1 -1 -2 10)
    (5 -2 -1 -1 -1 -1 -2 5)
    (5 -2 -1 -1 -1 -1 -2 5)
    (10 -2 -1 -1 -1 -1 -2 10)
    (-20 -50 -2 -2 -2 -2 50 40)
    (100 -20 10 5 5 10 40 100)))
;11 18
(defparameter *pesos6* 
  '((100 40 10 5 5 10 40 100)
    (40 50 -2 -2 -2 -2 50 40)
    (10 -2 -1 -1 -1 -1 -2 10)
    (5 -2 -1 -1 -1 -1 -2 5)
    (5 -2 -1 -1 -1 -1 -2 5)
    (10 -2 -1 -1 -1 -1 -2 10)
    (-20 -50 -2 -2 -2 -2 -50 -20)
    (100 -20 10 5 5 10 -20 100)))
;11 81
(defparameter *pesos7* 
  '((100 40 10 5 5 10 -20 100)
    (40 50 -2 -2 -2 -2 -50 -20)
    (10 -2 -1 -1 -1 -1 -2 10)
    (5 -2 -1 -1 -1 -1 -2 5)
    (5 -2 -1 -1 -1 -1 -2 5)
    (10 -2 -1 -1 -1 -1 -2 10)
    (40 50 -2 -2 -2 -2 -50 -20)
    (100 40 10 5 5 10 -20 100)))
;11 88
(defparameter *pesos8* 
  '((100 40 10 5 5 10 -20 100)
    (40 50 -2 -2 -2 -2 -50 -20)
    (10 -2 -1 -1 -1 -1 -2 10)
    (5 -2 -1 -1 -1 -1 -2 5)
    (5 -2 -1 -1 -1 -1 -2 5)
    (10 -2 -1 -1 -1 -1 -2 10)
    (-20 -50 -2 -2 -2 -2 50 40)
    (100 -20 10 5 5 10 40 100)))
;18 81
(defparameter *pesos9* 
  '((100 -20 10 5 5 10 40 100)
    (-20 -50 -2 -2 -2 -2 50 40)
    (10 -2 -1 -1 -1 -1 -2 10)
    (5 -2 -1 -1 -1 -1 -2 5)
    (5 -2 -1 -1 -1 -1 -2 5)
    (10 -2 -1 -1 -1 -1 -2 10)
    (40 50 -2 -2 -2 -2 -50 -20)
    (100 40 10 5 5 10 -20 100)))
;18 88
(defparameter *pesos10* 
  '((100 -20 10 5 5 10 40 100)
    (-20 -50 -2 -2 -2 -2 50 40)
    (10 -2 -1 -1 -1 -1 -2 10)
    (5 -2 -1 -1 -1 -1 -2 5)
    (5 -2 -1 -1 -1 -1 -2 5)
    (10 -2 -1 -1 -1 -1 -2 10)
    (-20 -50 -2 -2 -2 -2 50 40)
    (100 -20 10 5 5 10 40 100)))
;81 88
(defparameter *pesos11* 
  '((100 -20 10 5 5 10 -20 100)
    (-20 -50 -2 -2 -2 -2 -50 -20)
    (10 -2 -1 -1 -1 -1 -2 10)
    (5 -2 -1 -1 -1 -1 -2 5)
    (5 -2 -1 -1 -1 -1 -2 5)
    (10 -2 -1 -1 -1 -1 -2 10)
    (40 50 -2 -2 -2 -2 50 40)
    (100 40 10 5 5 10 40 100)))

;18 81 88
(defparameter *pesos12* 
  '((100 -20 10 5 5 10 40 100)
    (-20 -50 -2 -2 -2 -2 50 40)
    (10 -2 -1 -1 -1 -1 -2 10)
    (5 -2 -1 -1 -1 -1 -2 5)
    (5 -2 -1 -1 -1 -1 -2 5)
    (10 -2 -1 -1 -1 -1 -2 10)
    (40 50 -2 -2 -2 -2 50 40)
    (100 40 10 5 5 10 40 100)))
;11 81 88
(defparameter *pesos13* 
  '((100 40 10 5 5 10 -20 100)
    (40 50 -2 -2 -2 -2 -50 -20)
    (10 -2 -1 -1 -1 -1 -2 10)
    (5 -2 -1 -1 -1 -1 -2 5)
    (5 -2 -1 -1 -1 -1 -2 5)
    (10 -2 -1 -1 -1 -1 -2 10)
    (40 50 -2 -2 -2 -2 50 40)
    (100 40 10 5 5 10 40 100)))
;11 18 88
(defparameter *pesos14* 
  '((100 40 10 5 5 10 40 100)
    (40 50 -2 -2 -2 -2 50 40)
    (10 -2 -1 -1 -1 -1 -2 10)
    (5 -2 -1 -1 -1 -1 -2 5)
    (5 -2 -1 -1 -1 -1 -2 5)
    (10 -2 -1 -1 -1 -1 -2 10)
    (-20 -50 -2 -2 -2 -2 50 40)
    (100 -20 10 5 5 10 40 100)))
;11 18 81 
(defparameter *pesos15* 
  '((100 40 10 5 5 10 40 100)
    (40 50 -2 -2 -2 -2 50 40)
    (10 -2 -1 -1 -1 -1 -2 10)
    (5 -2 -1 -1 -1 -1 -2 5)
    (5 -2 -1 -1 -1 -1 -2 5)
    (10 -2 -1 -1 -1 -1 -2 10)
    (40 50 -2 -2 -2 -2 -50 -20)
    (100 40 10 5 5 10 -20 100)))

(defparameter *pesos16* 
  '((100 40 10 5 5 10 40 100)
    (40 50 -2 -2 -2 -2 50 40)
    (10 -2 -1 -1 -1 -1 -2 10)
    (5 -2 -1 -1 -1 -1 -2 5)
    (5 -2 -1 -1 -1 -1 -2 5)
    (10 -2 -1 -1 -1 -1 -2 10)
    (40 50 -2 -2 -2 -2 50 40)
    (100 40 10 5 5 10 40 100)))

(defun weightini-fn (player board)
  (let ((brd (get-board board)))
    (apply #'+ 
      (mapcar 
        #'(lambda (boardrow weightrow) (row-value player boardrow weightrow))
        brd *pesosini*))))

(defun weight2-fn (player board)
  (let ((brd (get-board board)))
    (apply #'+ 
      (mapcar 
        #'(lambda (boardrow weightrow) (row-value player boardrow weightrow))
        brd *pesos2*))))

(defun weight3-fn (player board)
  (let ((brd (get-board board)))
    (apply #'+ 
      (mapcar 
        #'(lambda (boardrow weightrow) (row-value player boardrow weightrow))
        brd *pesos3*))))

(defun weight4-fn (player board)
  (let ((brd (get-board board)))
    (apply #'+ 
      (mapcar 
        #'(lambda (boardrow weightrow) (row-value player boardrow weightrow))
        brd *pesos4*))))

(defun weight5-fn (player board)
  (let ((brd (get-board board)))
    (apply #'+ 
      (mapcar 
        #'(lambda (boardrow weightrow) (row-value player boardrow weightrow))
        brd *pesos5*))))

(defun weight6-fn (player board)
  (let ((brd (get-board board)))
    (apply #'+ 
      (mapcar 
        #'(lambda (boardrow weightrow) (row-value player boardrow weightrow))
        brd *pesos6*))))

(defun weight7-fn (player board)
  (let ((brd (get-board board)))
    (apply #'+ 
      (mapcar 
        #'(lambda (boardrow weightrow) (row-value player boardrow weightrow))
        brd *pesos7*))))

(defun weight8-fn (player board)
  (let ((brd (get-board board)))
    (apply #'+ 
      (mapcar 
        #'(lambda (boardrow weightrow) (row-value player boardrow weightrow))
        brd *pesos8*))))

(defun weight9-fn (player board)
  (let ((brd (get-board board)))
    (apply #'+ 
      (mapcar 
        #'(lambda (boardrow weightrow) (row-value player boardrow weightrow))
        brd *pesos9*))))

(defun weight10-fn (player board)
  (let ((brd (get-board board)))
    (apply #'+ 
      (mapcar 
        #'(lambda (boardrow weightrow) (row-value player boardrow weightrow))
        brd *pesos10*))))

(defun weight11-fn (player board)
  (let ((brd (get-board board)))
    (apply #'+ 
      (mapcar 
        #'(lambda (boardrow weightrow) (row-value player boardrow weightrow))
        brd *pesos11*))))

(defun weight12-fn (player board)
  (let ((brd (get-board board)))
    (apply #'+ 
      (mapcar 
        #'(lambda (boardrow weightrow) (row-value player boardrow weightrow))
        brd *pesos12*))))

(defun weight13-fn (player board)
  (let ((brd (get-board board)))
    (apply #'+ 
      (mapcar 
        #'(lambda (boardrow weightrow) (row-value player boardrow weightrow))
        brd *pesos13*))))

(defun weight14-fn (player board)
  (let ((brd (get-board board)))
    (apply #'+ 
      (mapcar 
        #'(lambda (boardrow weightrow) (row-value player boardrow weightrow))
        brd *pesos14*))))

(defun weight15-fn (player board)
  (let ((brd (get-board board)))
    (apply #'+ 
      (mapcar 
        #'(lambda (boardrow weightrow) (row-value player boardrow weightrow))
        brd *pesos15*))))

(defun weight16-fn (player board)
  (let ((brd (get-board board)))
    (apply #'+ 
      (mapcar 
        #'(lambda (boardrow weightrow) (row-value player boardrow weightrow))
        brd *pesos16*))))

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

(defun corner-ocuped-check (player board)
  (let ((brd (get-board board)))
    (if (and (and (or (= (first (first brd)) 2) (= (first (first brd)) 1)) 
                  (or (= (first (octavo brd)) 2) (= (first (octavo brd)) 1)))
             (and (or (= (octavo (first brd)) 2) (= (octavo (first brd)) 1)) 
                  (or (= (octavo (octavo brd)) 2) (= (octavo (octavo brd)) 1))))
      t)))

(defun corner-UL-check (player board)
  (let ((brd (get-board board)))
    (if (or (= (first (first brd)) 2) (= (first (first brd)) 1))          
      t)))

(defun corner-UR-check (player board)
  (let ((brd (get-board board)))
    (if (or (= (octavo (first brd)) 2) (= (octavo (first brd)) 1))          
      t)))

(defun corner-DL-check (player board)
  (let ((brd (get-board board)))
    (if (or (= (first (octavo brd)) 2) (= (first (octavo brd)) 1))          
      t)))

(defun corner-DR-check (player board)
  (let ((brd (get-board board)))
    (if (or (= (octavo (octavo brd)) 2) (= (octavo (octavo brd)) 1))          
      t)))

(defun octavo(L)
  (second (cdr (cdr (cdr (cdr (cdr (cdr L))))))))