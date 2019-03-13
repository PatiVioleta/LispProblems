(DEFUN st (L nv nm)
    (COND
        ((NULL L) NIL)
        ((= nv (+ 1 nm)) NIL)
        (T (CONS (CAR L) (CONS (CADR L) (st (CDDR L) (+ 1 nv) (+ (CADR L) nm))) ))
    )
)
(DEFUN stang (L) (st (CDDR L) 0 0))


(DEFUN dr (L nv nm)
    (COND
        ((NULL L) NIL)
        ((= nv (+ 1 nm)) L)
        (T (dr (CDDR L) (+ 1 nv) (+ (CADR L) nm)))
    )
)
(DEFUN drept (L) (dr (CDDR L) 0 0))

(defun convert(L)
	(cond
		((null L)NIL)
		(T (cons (car L) (list (convert (stang L))(convert (drept L)))))
	)
)