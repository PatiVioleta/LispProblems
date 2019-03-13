;a)
(defun dublare(L N)
	(cond
		( (null L) NIL)
		( (= N 1) (cons (car L) L))
		(T (cons (car L) (dublare (cdr L) (- N 1))))
	)
)

;b)
(defun asociere(A B)
	(cond
		((null A) B)
		((null B) A)
		(T (cons (cons (car A) (car B)) (asociere (cdr A) (cdr B)) ))
	)
)

;c)
(defun lis_a(L)
	(cond
		( (null L) 0)
		( (atom (car L)) (lis_a (cdr L)))
		( (not (null (lis_a (car L))))  (+ 1 (+ (lis_a (car L)) (lis_a (cdr L)))))
		( T (+ 1 (lis_a (cdr L))))
	)
)

(defun lis(L) (+ 1 (lis_a L)))

;d)
(defun nrat(L)
	(cond
		((null L) 0)
		((atom (car L)) (+ 1 (nrat (cdr L))))
		(T (nrat (cdr L)))
	)
)