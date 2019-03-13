(defun nivel(L)
	(cond
		((null L) 0)
		(T (max (+ 1 (nivel (cadr L))) (+ 1 (nivel (caddr L)))))
	)
)