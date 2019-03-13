(defun power (x y) (if (= y 0) 1 (* x (power x (1- y)))))

(DEFUN cmmdcLin(L)
    (COND
        ((= (LENGTH L) 0) NIL)
        ((= (LENGTH L) 1) (CAR L))
        (T (cmmdc (CAR L) (cmmdcLin (CDR L) )))
    )
)

(DEFUN primul(L)
    (COND
        ((NULL L) NIL)
        ((AND (ATOM (CAR L)) (NULL(NULL (CAR L)))) (CAR L))
        ((AND (ATOM (CAR L)) (NULL (CAR L))) (primul (CDR L)))
        (T (primul (CAR L)))
    )
)