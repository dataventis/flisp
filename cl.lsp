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
    ((eq result-type 'list)
     ;; Fake map with just unary function
     (mapcar f (car lists))
     )
    (t (throw invalid-value
	 "(map result-type function[ list..]) - result-type unsupported"
	 result-type ))))
(defun identity (o) o)

;;; CL: (merge type l1 l2 p[ key])
;;; (merge l1 l2 p[ key])
(defun merge (l1 l2 p . opt)
  (let ((key  (if opt (car opt)  identity)))
    (let loop ((l1 l1) (l2 l2) (r nil))
	 (cond
	   ((and (null l1) (null l2))  (reverse r))
	   ((null l1)  (append (reverse r) l2))
	   ((null l2)  (append (reverse r) l1))
	   ((p (key (car l1)) (key (car l2))) (loop (cdr l1) l2 (cons (car l1) r)))
	   ((p (key (car l2)) (key (car l1))) (loop l1 (cdr l2) (cons (car l2) r)))
	   (t  (loop (cdr l1) (cdr l2) (cons (car l1) (cons (car l2) r)))) ))))

;;; Not part of Common Lisp
(defun nfirst (i l)
  (cond
    ((i= 0 i)  nil)
    ((null l) nil)
    (t  (cons (car l) (nfirst (i- i 1) (cdr l)))) ))

;;; (subseq l start[ end])
(defun subseq (l start . opt)
  (let ((end (if opt (car opt) (length l))))
    (nfirst (- end start) (nthcdr start l)) ))


;;; (sort l p[ key])
(defun sort (l p . opt)
  (let ((key (if opt (car opt) identity)))
	(if (or (null l) (null (cdr l))) l
	    (merge
	     (sort (nthcdr (i/ (length l) 2) l) p key)
	     (sort (nfirst (i/ (length l) 2) l) p key) p key) )))
