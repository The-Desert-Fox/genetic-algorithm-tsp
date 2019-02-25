(defun collect-until (str index)
	(map 'string #'(lambda (i) (char str i))(loop for x from 0 to index collect x)))
(defun collect-from (str index)
	(map 'string #'(lambda (i) (char str i))(loop for x from index to (1- (length str)) collect x)))
(defun extract-until (str) 
	(collect-until str (1- (position #\Space str))))
(defun extract-from (str)
	(collect-from str (1+ (position #\Space str))))
(defun split (str)
	(list (extract-until str) (extract-until (extract-from str)) (extract-from (extract-from str))))

(defvar *numnodes*)
(defvar *matrix*)
(with-open-file (distance "distance" :direction :input :if-does-not-exist :error)
	(when distance
		(setf *numnodes* (parse-integer (read-line distance nil nil)))
		(setf *matrix* (make-array (list *numnodes* *numnodes*)))
		(loop for line = (read-line distance nil nil) do 
			(if line
				(let ((lst (map 'list #'(lambda (i) (parse-integer i)) (split line))))
					(setf (aref *matrix* (1- (first lst)) (1- (second lst))) (third lst)))
				(return)))))
