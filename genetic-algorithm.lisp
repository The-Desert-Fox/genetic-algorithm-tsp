(load "graph.lisp" :if-does-not-exist :error)

(defconstant population-size 100)
(defconstant generations 100)
(defconstant nodes (loop for x from 0 to (1- *numnodes*) collect x))
(defun random-index (path)
	(nth (random (length path)) path))
(defun random-path (path)
	(when path
		(let ((chosen (random (length path)))) 
			(cons (nth chosen path) 
						(random-path (remove (nth chosen path) path))))))
(defun distance (path)
	(when (< 1 (length path))
		(cons (aref *matrix* (first path) (second path)) 
					(distance (remove (first path) path)))))
(defun cost (path)
	(if path
		(+ (first path) 
			 (cost (remove (first path) path)))
		0))
(defun init ()
	(sort 
		(loop for x from 1 to population-size 
					collect (let ((path (random-path nodes))) 
						(list (cost (distance path)) path))) #'< :key #'first))
(defun selection (population)
	(map 'list
		#'(lambda (x) 
				(let ((pos (position x population)))
					(when (< pos (random population-size))
					x)))
		population))
(defun crossover (dad mom)
	(let ((size (floor (/ *numnodes* 2))))
		(concatenate 'list 
		(loop for x from 0 to size 
					collect (nth x dad)) 
		(loop for y from (1+ size) to (1- *numnodes*) 
					collect (nth y mom)))))
(defun mutate (path)
	(if (> 20 (random 100))
		(list (first path)
					(loop for x in path 
								collect (if (= 0 x) 1
													(- x 1))))
		path))
(defun mutation (population)
	(let ((parents 
					(loop for path in (remove nil population) 
								collect (second path)))
		(loop for x in population 
					collect (if x x
						(let* ((dad (random-index parents))
									 (mom (random-index parents))
									 (son (mutate (crossover dad mom))))
							(list (cost (distance son)) son)))))))
(defun solve (population generation)
	(if (> generation 0)
		(solve (sort (mutation (selection population)) #'< #'first) (1- generation))
		(let* ((final (first (sort (mutation (selection population)) #'< #'first))) 
					 (cost (first final)) 
					 (solution (second final)))
			(princ "Cost: ")
			(princ cost)
			(terpri)
			(princ "Solution: ")
			(princ solution))))
(solve (init) generations)
