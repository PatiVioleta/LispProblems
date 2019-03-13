(defun apare(L E)
	(cond
		((null L)nil)
		((equal (car L) E) T)
		((atom (car L)) (apare (cdr L) E))
		(T (or (apare (car L) E)(apare (cdr L) E)))
	)
)

(defun elimina(E L)
	(cond
		((null L)nil)
		((equal (car L) E) (cdr L))
		((atom (car L)) (cons (car l)(elimina E (cdr L))))
		((apare (car L) E) (cons (elimina E (car L)) (cdr L)))
		(T (cons (car L)(elimina E (cdr L))))
	)
)