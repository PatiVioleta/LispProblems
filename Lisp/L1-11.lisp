;11 b
(defun munte_aux(L D)
	(cond
		((and (null (cdr L)) (or(= D 1)(= D 0)))NIL)
		((and (null (cdr L)) (= D 2))T)
		((and(>(car L)(cadr L))(= D 0)) NIL)
		((and(<(car L)(cadr L))(or(= D 1)(= D 0))) (munte_aux (cdr L) 1) )
		((and(>(car L)(cadr L))(= D 1)) (munte_aux (cdr L) (+ D 1)) )
		((and(>(car L)(cadr L))(= D 2)) (munte_aux (cdr L) 2) )
		(T NIL)
	)
)
(defun munte(L) (munte_aux L 0))