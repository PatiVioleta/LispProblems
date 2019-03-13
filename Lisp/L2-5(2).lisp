(defun apare(L A)
	(cond
		((null L) NIL)
		((and (atom (car L))(equal (car L) A)) T)
		((atom (car L)) (apare (cdr L) A))
		(T (or (apare (car L) A) (apare (cdr L) A)))
	)
)

(defun adancime(L N)
	(cond
		((null L) 0)
		((equal(car L)N) 1)
		((apare (cadr L) N) (+ 1 (adancime (cadr L) N)))
		((apare (caddr L) N) (+ 1 (adancime (caddr L) N)))
		(T 0)
	)
)