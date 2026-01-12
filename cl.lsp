(setq first car)
(setq rest cdr)
(setq second cadr)
(defun 1+ (n) (+ 1 n))
(defun endp (l)
  (cond
    ((null l))
    ((consp l) nil)
    (t (throw invalid-value "(endp l) - l is neither list nor nil" l)) ))
(defun map (result-type function . lists)
  (cond
    ((null result-type) nil)
    ((eq result-type type-list)
     ;; Fake map with just unary function
     (mapcar f (car lists))
     )
    (t (throw invalid-value
	 "(map result-type function[ list..]) - result-type unsupported"
	 result-type ))))
(defun merge (l1 l2 p . opt)
  (let ((key  (if opt (car opt)  ())))
    (let loop ((l1 l1) (l2 l2) (r nil))
	 (cond
	   ((and (null l1) (null l2))  (reverse r))
	   ((null l1)  (append (reverse r) l2))
	   ((null l2)  (append (reverse r) l1))
	   ;; Note: not using key function
	   ((p (car l1) (car l2)) (loop (cdr l1) l2 (cons (car l1) r)))
	   ((p (car l2) (car l1)) (loop l1 (cdr l2) (cons (car l2) r)))
	   (t  (loop (cdr l1) (cdr l2) (cons (car l1) (cons (car l2) r)))) ))))
    
