;b)
(defun invers(L)
	(cond
		((null L) NIL)
		( (atom (car L)) (append (invers (cdr L)) (list (car L)) ))
		;( (null (cdr L)) (list (invers (car L))))
		(T (append (invers (cdr L)) (list (invers (car L)))))
	)
)

;c)
(defun nrat(L)
	(cond
		((null L) 0)
		((atom (car L)) (+ 1 (nrat (cdr L))))
		(T (nrat (cdr L)))
	)
)

(defun prat(L)
	(cond
		((null L) NIL)
		((atom (car L)) (car L))
		(T (prat (cdr L)))
	)
)

(defun prli(L)
	(cond
		((null L) NIL)
		((atom (car L)) (prli (cdr L)))
		(T (car L))
	)
)

(defun princi(L)
	(cond
		((null L) NIL)
		((atom (car L)) (princi (cdr L)))
		((= 1 (mod (nrat (car L)) 2))  (append (list (prat (car L))) (princi (car L)) (princi (list (prli (cdr L))))))
		(T (append (princi (car L)) (princi (list (prli (cdr L))))))
	)
)

(defun princi0(L)
	(cond
		((= 1 (mod (nrat L) 2)) (cons (prat L) (princi L)))
		(T (princi L))
	)
)
