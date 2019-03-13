(defun atomi(L)
	(cond
		((null L)NIL)
		((atom (car L)) (cons (car L) (atomi (cdr L))))
		(T (atomi (cdr L)))
	)
)

(defun nivel(L K)
	(cond
		((null L) NIL)
		((= K 0) (atomi L))
		(T (append (nivel (cadr L) (- K 1)) (nivel (caddr L) (- K 1))))
	)
)