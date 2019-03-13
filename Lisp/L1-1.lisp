(DEFUN insereaza_aux(L A D)
    (COND
        ((= D 0) ( CONS A (insereaza_aux L A 2)))
        ((NULL L) NIL)
        ((> D 0) ( CONS (CAR L) (insereaza_aux (CDR L) A (- D 1)))) 
    )
)

(DEFUN insereaza(L A) (insereaza_aux L A 2))

(DEFUN atomi(L)
    (COND
        ((NULL L) NIL)
        ((AND (ATOM (CAR L)) (NOT (EQUAL (CAR L) NIL))) (append (atomi (CDR L)) (LIST (CAR L))))
        (T (append (atomi (CDR L)) (atomi (CAR L))))
    )
)

(DEFUN cmmdc(A B)
    (COND
        ((= A 0) B)
        ((= B 0) A)
        ((EQUAL A B) A)
        ((> A B) (cmmdc (- A B) B))
        (T (cmmdc A (- B A)))
    )
)

(DEFUN cmmdcLin(L)
    (COND
        ((AND (= (LENGTH L) 1) (NUMBERP (CAR L)))  (CAR L))
        ((AND (= (LENGTH L) 1) (ATOM (CAR L)) )  0)
        ((NOT (NUMBERP (CAR L))) (cmmdcLin (CDR L)))
        (T (cmmdc (CAR L) (cmmdcLin (CDR L) )))
    )
)

(DEFUN cmmdcL(L) (cmmdcLin (ATOMI L)))

(DEFUN nrAparLin(L E)
    (COND
        ((NULL L) 0)
        ((EQUAL (CAR L) E) (+ 1 (nrAparLin (CDR L) E)))
        (T (nrAparLin (CDR L) E))
    )
)

(DEFUN nrAparitii(L E) (nrAparLin (ATOMI L) E))
