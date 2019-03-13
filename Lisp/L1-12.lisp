;12 c
(defun inser_poz(L A P)
	(cond
		((and(null L)(= P 1))(list A))
		((null L) NIL)
		((= P 1) (cons A L))
		(T (cons (car L) (inser_poz (cdr L) A (- P 1))))
	)
)


(defun insereaza(L A K)
	(cond
		((= K 0) NIL)
		(T (cons (inser_poz L A K)(insereaza L A (- K 1))))
	)
)

(defun insereaza_liste(L A)
	(cond
		((null L) NIL)
		(T (append (insereaza (car L) A (+ 1(length (car L)))) (insereaza_liste (cdr L) A)))
	)
)

(defun permutari(L)
	(cond
		((null L)NIL)
		((null (cdr L)) (list (list (car L))))
		(T (insereaza_liste (permutari (cdr L)) (car L)))
	)
)