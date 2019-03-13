;a)
(defun produs(A B)
	(cond
		((null a) 0)
		(T (+ (* (car A) (car B)) (produs (cdr A) (cdr B)) ))
	)
)

(defun produs_2(A B)
	(apply #'+ (mapcar #'* A B))
)

;b)
(defun adancime(L)
	(cond
		( (null L) 0)
		( (and (atom (car L)) (null (car L))) 1)
		( (atom (car L)) (adancime (cdr L)))
		( T (max (+ 1 (adancime (car L))) (adancime (cdr L)) ))
	)
)

;c)
(defun ad(L A)
	(cond
		((null L) (list A))
		( (< A (car L)) (cons A L))
		( (= A (car L)) L)
		( T (cons (car L) (ad (cdr L) A)))
	)
)

(defun sorte(L)
	(cond
		((null L) NIL)
		( T (ad (sorte (cdr L)) (car L)))
	)
)

;d)
(defun membru(L A)
	(cond
		((null L) nil)
		( (equal (car L) A) T)
		( (atom (car L)) (membru (cdr L) A) )
		( T (or (membru (car L) A) (membru (cdr L) A)))
	)
)

(defun elimina(L A)
	(cond
		((null L) nil)
		( (equal (car L) A) (elimina (cdr L) A))
		( T (cons (car L) (elimina (cdr L) A)))
	)
)

(defun inters(A B)
	(cond
		((null A) NIL)
		( (membru B (car A)) (cons (car A) (inters (cdr A) (elimina B (car A))) ))
		(T (inters (cdr A) B))
	)
)
