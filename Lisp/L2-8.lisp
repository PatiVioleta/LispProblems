(defun inord(L)
	(cond
		((null L)NIL)
		(T (append (inord (cadr L)) (list (car L)) (inord (caddr L))))
	)
)