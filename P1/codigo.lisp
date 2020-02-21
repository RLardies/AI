;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun newton (f df-dx max-iter x0 &optional (tol-abs 0.0001))
  "Zero of a function using the Newton-Raphson method


    INPUT:  f:        function whose zero we wish to find
            df-dx:    derivative of f
            max-iter: maximum number of iterations 
            x0:       initial estimation of the zero (seed)
            tol-abs:  tolerance for convergence


    OUTPUT: estimation of the zero of f, NIL if not converged"
    (let ((df-dx-x0 (funcall df-dx x0))) ;; Calculamos el valor de la derivada en el punto
    (if (> (abs df-dx-x0) 1.0L-15) ;; Si la derivada es 0, devolver NIL
      (let* ((f-x0 (funcall f x0)) (xn1 (- x0 (/ f-x0 df-dx-x0))))
        (cond ((< max-iter 0) NIL) ((< (abs (- xn1 x0)) tol-abs) xn1)
          (t (newton f df-dx (- max-iter 1) xn1 tol-abs)))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun newton-all (f df-dx max-iter seeds &optional (tol-abs 0.0001))
  "Zeros of a function using the Newton-Raphson method


    INPUT:  f:        function whose zero we wish to find
            df-dx:    derivative of f
            max-iter: maximum number of iterations 
            seeds:    list of initial estimations of the zeros 
            tol-abs:  tolerance for convergence


    OUTPUT: list of estimations of the zeros of f"
    (mapcar #'(lambda (x0) (newton f df-dx max-iter x0 tol-abs)) seeds))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun combine-elt-lst (elt lst)
  "Combines an element with all the elements of a list


    INPUT:  elt: element 
            lst: list 


    OUTPUT: list of pairs, such that 
               the first element of the pair is elt. 
               the second element is an element from lst"
  (mapcar #'(lambda (x) (list el  x)) lst)) 
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun combine-list-of-lsts (lolsts)
  "Combinations of N elements, each of wich


   INPUT:  lstolsts: list of N sublists (list1 ... listN)


   OUTPUT: list of sublists of N elements, such that in each 
           sublist the first element is from list1
                 the second element is from list 2
                 ...
                 the Nth element is from list N"
  (mapcan #'(lambda (x) (combine-elt-lst x lst2)) lst1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun scalar-product (x y)
  "Calculates the scalar product of two vectors
 
   INPUT:  x: vector, represented as a list
           y: vector, represented as a list
 
   OUTPUT: scalar product between x and y


   NOTES: 
        * Implemented with mapcar"
  (apply #'+ (mapcar #'* x y)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; euclidean-norm


(defun euclidean-norm (x)
  "Calculates the euclidean (l2) norm of a vector
   
    INPUT:  x: vector, represented as a list


    OUTPUT: euclidean norm of x"
  (sqrt (apply #'+ (mapcar #'(lambda (x0) (* x0 x0)) x))))


________________


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; euclidean-distance


(defun euclidean-distance (x y) 
  "Calculates the euclidean (l2) distance between two vectors
 
    INPUT: x: vector, represented as a list
           y: vector, represented as a list


    OUTPUT: euclidean distance between x and y"
  (euclidean-norm (mapcar #'- x y)))
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun cosine-similarity (x y) 
  "Calculates the cosine similarity between two vectors


    INPUT:  x: vector, representad as a list
            y: vector, representad as a list


    OUTPUT: cosine similarity between x and y


    NOTES: 
       * Evaluates to NIL (not defined)
         if at least one of the vectors has zero norm.
       * The two vectors are assumed to have the same length"
  (let ((scal-product (scalar-product x y)) 
    (norm-product (* (euclidean-norm x) (euclidean-norm y))))
    (if (/= 0 norm-product) ;; Si el producto de las normas es 0, devolver NIL
      (/ scal-product norm-product ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun angular-distance (x y) 
  "Calculates the angular distance between two vectors


   INPUT:  x: vector, representad as a list
           y: vector, representad as a list


   OUTPUT: cosine similarity between x and y


   NOTES: 
      * Evaluates to NIL (not well defined)
        if at least one of the vectors has zero norm.
      * The two vectors are assumed to have the same length"
  (let ((cos-sim (cosine-similarity x y)))
    (if (null cos-sim)
      NIL
      (/ (acos cos-sim) pi))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; select-vectors


(defun select-vectors (lst-vectors test-vector similarity-fn &optional (threshold 0))
    "Selects from a list the vectors whose similarity to a 
     test vector is above a specified threshold. 
     The resulting list is ordered according to this similarity.
 
     INPUT:  lst-vectors:   list of vectors
             test-vector:   test vector, representad as a list
             similarity-fn: reference to a similarity function
             threshold:     similarity threshold (default 0)
      
     OUTPUT: list of pairs. Each pair is a list with
             a vector and a similarity score.
             The vectors are such that their similarity to the 
             test vector is above the specified threshold.
             The list is ordered from larger to smaller 
             values of the similarity score 
     
     NOTES: 
        * Uses remove-if and sort"
  
    (/ (scalar-product x y) (* (euclidean-norm x) (euclidean-norm y))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


________________


(defun nearest-neighbor (lst-vectors test-vector distance-fn)
  "Selects from a list the vector that is closest to the 
   reference vector according to the specified distance function 
 
   INPUT:  lst-vectors:   list of vectors
           ref-vector:    reference vector, represented as a list
           distance-fn:   reference to a distance function
      
   OUTPUT: List formed by two elements:
           (1) the vector that is closest to the reference vector 
               according to the specified distance function
           (2) The corresponding distance value.


   NOTES: 
      * The implementation is recursive
      * It ignores the vectors in lst-vectors for which the 
        distance value cannot be computed."
  (sort (remove-if #'(lambda (x) (< (second x) threshold))
    (mapcar #'(lambda (lst) 
      (list lst (funcall similarity-fn lst test-vector))) lst-vectors)) #'(lambda (x y)
       (> (second x) (second y)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun backward-chaining (goal lst-rules)
  "Backward-chaining algorithm for propositional logic
 
   INPUT: goal:      symbol that represents the goal
          lst-rules: list of pairs of the form 
                     (<antecedent>  <consequent>)
                     where <antecedent> is a list of symbols
                     and  <consequent> is a symbol


   OUTPUT: T (goal derived) or NIL (goal cannot be derived)


   NOTES: 
        * Implemented with some, every" 


  (backward-chaining-aux goal lst-rules NIL))


(defun backward-chaining-aux (goal lst-rules pending-goals)
  (if (member goal pending-goals) NIL
    (some #'(lambda (x) (if (equal (second x) goal) (if (null (car x)) t
      (every #'(lambda (y) 
        (backward-chaining-aux y (remove x lst-rules) (cons goal pending-goals))) (car x)))))
    lst-rules)))






________________


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Breadth-first-search in graphs
;;;
(defun bfs (end queue net)
  (if (null queue) 
      NIL
    (let* ((path (first queue))
           (node (first path)))
      (if (eql node end) 
          (reverse path)
        (bfs end 
             (append (rest queue) 
                     (new-paths path node net)) 
             net))))) 


 (defun new-paths (path node net)
  (mapcar #'(lambda(n) 
        (cons n path)) 
                (rest (assoc node net))))
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun shortest-path (start end net)
  (bfs end (list (list start)) net))    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(defun bfs-improved (end queue net)
  (if (null queue) 
      NIL
    (let* ((path (first queue))
           (node (first path)))
      (if (eql node end) 
          (reverse path)
        (bfs-improved end 
             (append (rest queue) 
                     (new-paths-improved path node net)) 
             net)))))

(defun new-paths-improved (path node net)
                  (add-vecinos path (rest (assoc node net))))

(defun add-vecinos (path vecinos)
  (mapcar #'(lambda(x)
    (add-to-path path x)) vecinos))


(defun add-to-path (path n)
  (if (null (member n path))
    (cons n path)))




(defun shortest-path-improved (end queue net)
    (bfs-improved end (list(list start)) net))
