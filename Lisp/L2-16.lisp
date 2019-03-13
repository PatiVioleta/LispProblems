(defun nivel(L)
	(cond
		((null L) 0)
		(T (max (+ 1 (nivel (cadr L))) (+ 1 (nivel (caddr L)))))
	)
)

(defun echilib(L)
	(cond
		((null L) T)
		((> (-(nivel (cadr L))(nivel (caddr L))) 1) NIL)
		((> (-(nivel (caddr L))(nivel (cadr L))) 1) NIL)
		(T (and (echilib (cadr L))(echilib (caddr L))))
	)
)