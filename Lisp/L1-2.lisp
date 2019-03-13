;a)
(defun select (L N)
	(cond
		( (null L) nil)
		( (= N 1) (car L))
		( T (select (cdr L) (- N 1)) )
	)
)

;b)
(defun membru(L A)
	(cond
		((null L) nil)
		( (equal (car L) A) T)
		( (atom (car L)) (membru (cdr L) A) )
		( T (or (membru (car L) A) (membru (cdr L) A)))
	)
)

;c)
(defun lis_a(L)
	(cond
		( (null L) NIL)
		( (atom (car L)) (lis_a (cdr L)))
		( (not (null (lis_a (car L))))  (cons (car L) (append (lis_a (car L)) (lis_a (cdr L)))))
		( T (cons (car L) (lis_a (cdr L))))
	)
)

(defun lis(L) (cons L (lis_a L)))

;d)
(defun multime( L)
	(cond
		( (null L) NIL)
		( (membru (cdr L) (car L)) (multime (cdr L)))
		( T (cons (car L) (multime (cdr L))))
	)
)