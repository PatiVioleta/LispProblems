(defun nr_elem(L)
	(cond
		((null L) 0)
		(T (+ 1 (nr_elem (cdr L))))
	)
)

(defun convert(L)
	(cond
		((null L) NIL)
		(T (append (list (car L) (nr_elem (cdr L))) (convert (cadr L)) (convert (caddr L))) )
	)
)