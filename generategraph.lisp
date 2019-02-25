(defvar *numnodes* (read))
(with-open-file (distance "distance" :direction :output :if-exists :supersede)
	(princ *numnodes* distance) (terpri distance)
	(loop for i from 1 to *numnodes* do
				(loop for j from 1 to *numnodes* do
							(progn 
								(princ i distance) (princ "\ " distance)
								(princ j distance) (princ "\ " distance)
								(princ (+ 1 (random *numnodes*)) distance) (terpri distance)))))
