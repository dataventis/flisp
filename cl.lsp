;;; leg20260118: fLisp Common Lisp compatibility
;;;
;;; This file collects implementations and aliases of Common Lisp
;;; functions to increase compatibility with source written in that
;;; language. Compatibility is like in C: "less then 80% of the source
;;; has to be rewritten".
;;;
;;; Currently the provided functions are required by a certain
;;; implementation of huffman coding.
;;;
;;; Some functions are NOT compliant but share the same name with the
;;; respective Common Lisp function, others are helper functions and
;;; not part of Common Lisp: these are bugs.

(require 'flisp)

(setq first car)
(setq rest cdr)
(setq second cadr)
(setq third caddr)
(defun 1+ (n) (+ 1 n))
(defun endp (l)
  (cond
    ((null l))
    ((consp l) nil)
    (t (throw invalid-value "(endp l) - l is neither list nor nil" l)) ))

(setq identity progn)

;;; CL: (merge type l1 l2 p[ key])
;;; (merge l1 l2 p[ key])
(defun merge (l1 l2 p . opt)
  (let ((key  (if opt (car opt)  identity)))
    (let loop ((l1 l1) (l2 l2) (r nil))
	 (cond
	   ((and (null l1) (null l2))  (nreverse r))
	   ((null l1)  (append (nreverse r) l2))
	   ((null l2)  (append (nreverse r) l1))
	   ((p (key (car l1)) (key (car l2))) (loop (cdr l1) l2 (cons (car l1) r)))
	   ((p (key (car l2)) (key (car l1))) (loop l1 (cdr l2) (cons (car l2) r)))
	   (t  (loop (cdr l1) (cdr l2) (cons (car l1) (cons (car l2) r)))) ))))

;;; Not part of Common Lisp
;;; We should implement (last l[ n]) and (butlast l[ n]) instead
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

;;; Note: fake member, w/o key and test
(setq member memq)

;;; Note: not compliant: we do not return the mismatch index.
(defun string< (s1 s2)
  (i> 0 (string-compare s1 s2)) )

(provide 'cl)
