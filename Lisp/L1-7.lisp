;a)
(defun liniara(L)
	(cond
		((null L) T)
		( (not (atom (car L))) NIL )
		(T (liniara (cdr L)))
	)
)

;b)
(defun apar(L E1 E2)
	(cond
		((null L) NIL)
		((equal (car L) E1) (cons E2 (cdr L)))
		(T (cons (car L) (apar (cdr L) E1 E2)))
	)
)

;c)
(defun ultim(L)
	(cond
		((null L) NIL)
		( (and (null (cdr L)) (not(atom (car L)))) (ultim (car L)) )
		((null (cdr L)) (car L))
		(T (ultim (cdr L)))
	)
)

(defun reducere(L)
	(cond
		((null L) NIL)
		((atom (car L)) (cons (car L) (reducere (cdr L))))
		(T (cons (ultim (car L)) (reducere (cdr L))))
	)
)

;d)
(defun interclasare(A B)
	(cond
		((null A) B)
		((null B) A)
		( (< (car A) (car B)) (cons (car A) (interclasare (cdr A) B)) )
		( (= (car A) (car B)) (cons (car A) (interclasare (cdr A) (cdr B))) )
		( T (cons (car B) (interclasare A (cdr B))) )
	)
)