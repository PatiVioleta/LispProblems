(defun apare(L A)
	(cond
		((null L) NIL)
		((and (atom (car L))(equal (car L) A)) T)
		((atom (car L)) (apare (cdr L) A))
		(T (or (apare (car L) A) (apare (cdr L) A)))
	)
)

(defun cale(L X)
	(cond
		((null L) NIL)
		((equal (car L) X) (list X))
		((apare (cadr L) X) (cons (car L) (cale (cadr L) X)))
		((apare (caddr L) X) (cons (car L) (cale (caddr L) X)))
		(T NIL)
	)
)