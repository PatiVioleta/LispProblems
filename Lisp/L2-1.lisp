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


(DEFUN apare (L X)
    (COND
        ((NULL L) NIL)
        ((EQUAL X (CAR L)) T)
        (T (apare (CDDR L) X))
    )
)

(DEFUN cale (L X)
    (COND
        ((EQUAL X (CAR L))  (LIST X))
        ((apare (stang L) X) (CONS (CAR L) (cale (stang L) X)))
        ((apare (drept L) X) (CONS (CAR L) (cale (drept L) X)))
    )
)

;(cale '(A 2 B 2 NIL 0 D 1 E 2 NIL 0 F 2 G 0 H 0 C 0) 'F)