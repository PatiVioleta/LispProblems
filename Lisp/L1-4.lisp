;a)
(defun suma(A B)
	(cond
		((null a) NIL)
		(T (cons (+ (car A) (car B)) (suma (cdr A) (cdr B)) ))
	)
)

(defun suma2(A B)
	(mapcar #'+ A B)
)

;b)
(defun atomi(L)
	(cond
		((null L) NIL)
		((atom (car L)) (cons (car L) (atomi (cdr L))))
		( T (append (atomi (car L)) (atomi (cdr L))))
	)
)

;c)
(defun invers(L)
	(cond
		((null L) NIL)
		(T (append (invers (cdr L)) (list (car L))))
	)
)

(defun pr(L)
	(cond
		((null L) NIL)
		((atom (car L)) (cons (car L) (pr (cdr L))))
		(T NIL)
	)
)

(defun ul(L)
	(cond
		((null L) NIL)
		((atom (car L)) (ul (cdr L)))
		(T L)
	)
)

(defun inv(L)
	(cond
		((null L) NIL)
		( (atom (car L)) (append (invers (pr L)) (inv (ul L))))
		;( (atom (car L)) (invers (pr L)))
		;( (null (inv (cdr L))) (list (inv (car L))))
		(T (cons (inv (car L)) (inv (cdr L))))
	)
)

;d)
(defun  maximul(L)
	(cond
		((null L) 0)
		((not (atom (car L))) (max (maximul (car L)) (maximul (cdr L))))
		((numberp (car L)) (max (car L) (maximul (cdr L))))
		(T (maximul(cdr L)))
	)
)

(defun  maximul2(L)
	(cond
		((null L) 0)
		((not (atom (car L)))(maximul2 (cdr L)))
		((numberp (car L)) (max (car L) (maximul2 (cdr L))))
		(T (maximul2(cdr L)))
	)
)