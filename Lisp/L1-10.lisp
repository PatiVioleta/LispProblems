;10 b c
;b)
(defun pereche(L A)
	(cond
		((null L) NIL)
		(T (cons (list A (car L)) (pereche (cdr L) A)))
	)
)

(defun perechi(L)
	(cond
		((null L) NIL)
		(T (append (pereche (cdr L) (car L)) (perechi (cdr L))))
	)
)

;c)
(defun expr(L)
	(cond
		((null L) NIL)
		((and (not (numberp (car L))) (numberp (cadr L)) (numberp (caddr L))) 
				(cons (apply (car L) (list (cadr L) (caddr L))) (expr (cdddr L)))	)
		(T (cons (car L) (expr (cdr L))))
	)
)

(defun aplica(L)
	(cond
		((null (cdr L)) (car L))
		(T (aplica (expr L)))
	)
)