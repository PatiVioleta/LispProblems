;a)
(defun interclasare(A B)
	(cond
		((null A) B)
		((null B) A)
		( (< (car A) (car B)) (cons (car A) (interclasare (cdr A) B)) )
		( T (cons (car B) (interclasare A (cdr B))) )
	)
)

;b)
(defun inloc(L A L1)
	(cond
		((null L) NIL)
		( (equal (car L) A) (append L1 (inloc (cdr L) A L1)) )
		( (listp (car L)) (cons (inloc (car L) A L1) (inloc (cdr L) A L1)) )
		( T (cons (car L) (inloc (cdr L) A L1) ))
	)
)

;c)
(defun invers(L)
	(cond
		((null L) NIL)
		(T (append (invers (cdr L)) (list (car L))))
	)
)

(defun trans(A E)
	(cond
		((and (null A) (not (= E 0))) (list E))
		((null A) NIL)
		( (> (+ (car A) E) 9) (cons (- (+ (car A) E) 10) (trans (cdr A) 1)) )
		( T (cons (+ (car A) E) (trans (cdr A) 0)) )
	)
)

(defun suma_aux(A B)
	(cond
		((null A) B)
		((null B) A )
		(T (cons (car (trans A (car B))) (suma_aux (cdr (trans A (car B))) (cdr B))))
	)
)

(defun suma( A B) (invers (suma_aux (invers A) (invers B))))

(defun trans_nr(L R)
	(cond
		((null L) R)
		(T (trans_nr (cdr L) (+ (* R 10) (car L))))
	)
)

(defun suma2(A B) (trans_nr (suma A B) 0))

;d)
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