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


; A)

;primero comprueba que el argumento queue no es null para que pueda enpezar la busqueda por un nodo.
; A continuacion define las variables locales path ( que será el camino que va a seguir) y node 
;(que será el nodo que está visitando). Si el nodo a visitar es la meta (end) acabamos
;y devolvemos el camino dado la vuelta (reverse path). Si no volvemos a llamar a la funcion bfs (recursiva)
;con la misma meta (end), la cola (queue) añadiendo lo que devuelve la funcion new-paths, y la lista 
; de adyacencia del grafo.
	


 (defun new-paths (path node net)
  (mapcar #'(lambda(n) 
        (cons n path)) 
                (rest (assoc node net))))


;
;La funcion new paths añade al camino los hijos del nodo que se está visitando usando
;la función assoc que los busca en la lista de adyacencia y rest para no incluir el que
; se está visitando


;Con el grafo del ejemplo para buscar D empezando desde C, usando trace vemos que se producen 4 llamadas a bfs,
;y vemos como va cambiando la cola, donde se van añadiendo los nodos que se van visitando, primero ((C)),
; luego ((E C)), ((B E C)), y finalmente ((D B E C)).

;; CAPTURA


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


(defun shortest-path-improved (start end net)
	(bfs-improved end (list(list start)) net))


;El camino desde dt a cada vértice en este recorrido contiene el mínimo número de vértices. Es el camino más corto medido en número de vértices.
;Su nombre se debe a que expande uniformemente la frontera entre lo descubierto y lo no descubierto. Llega a los nodos de distancia k, sólo tras haber llegado a todos los nodos a distancia k-1.

;G)

;(shortest-path 'A ' H '((A B C D E) (B A D E F) (C A G) (D A B G H) (E A B G H) (F B H) (G C D E H) (H D E F G)))

; H)

; Caso problematico: 
;(shortest-path 'C 'F '((A B C D E) (B A D E) (C A G) (D A B G H) (E A B G H) (F H) (G C D E H) (H D E G)))
; grafo con ciclos sin solucion

