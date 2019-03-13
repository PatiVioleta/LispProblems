;2 c)
; Sa se construiasca lista tuturor sublistelor unei liste. 
;Prin sublista se intelege fie lista insasi, fie un element de pe orice nivel, 
;care este lista. Exemplu: (1 2 (3 (4 5) (6 7)) 8 (9 10)) =>
;( (1 2 (3 (4 5) (6 7)) 8 (9 10)) (3 (4 5) (6 7)) (4 5) (6 7) (9 10) ). 
(defun sub(L)
	(cond
		((null L) NIL)
		((atom (car L)) (sub (cdr L)))
		(T (append (list (car L)) (sub (car L)) (sub (cdr L))))
	)
)

(defun subm(L) (append (list L) (sub L)))

;4 c)
; Sa se scrie o functie care plecand de la o lista data ca argument, inverseaza 
;numai secventele continue de atomi. Exemplu: (a b c (d (e f) g h i)) ==> 
;(c b a (d (f e) i h g))
(defun invers(L)
	(cond
		((null L) NIL)
		(T (append (invers (cdr L)) (list (car L))))
	)
)

(defun pr(L)
	(cond
		((null L) NIL)
		((atom (car L)) (cons (car L) (pr (cdr L))))
		(T NIL)
	)
)

(defun ul(L)
	(cond
		((null L) NIL)
		((atom (car L)) (ul (cdr L)))
		(T L)
	)
)

(defun inv(L)
	(cond
		((null L) NIL)
		( (atom (car L)) (append (invers (pr L)) (inv (ul L))))
		(T (cons (inv (car L)) (inv (cdr L))))
	)
)

;7 c)
;Sa se inlocuiasca fiecare sublista a unei liste cu ultimul ei element. 
;Prin sublista se intelege element de pe primul nivel, care este lista. 
;Exemplu: (a (b c) (d (e (f)))) ==> (a c f )
(defun ultim(L)
	(cond
		((null L) NIL)
		( (and (null (cdr L)) (not(atom (car L)))) (ultim (car L)) )
		((null (cdr L)) (car L))
		(T (ultim (cdr L)))
	)
)

(defun reducere(L)
	(cond
		((null L) NIL)
		((atom (car L)) (cons (car L) (reducere (cdr L))))
		(T (cons (ultim (car L)) (reducere (cdr L))))
	)
)

;9 b)
;Definiti o functie care inverseaza o lista impreuna cu toate 
;sublistele sale de pe orice nivel. 
(defun invers(L)
	(cond
		((null L) NIL)
		( (atom (car L)) (append (invers (cdr L)) (list (car L)) ))
		;( (null (cdr L)) (list (invers (car L))))
		(T (cons (invers (cdr L)) (list (invers (car L)))))
	)
)

;9 c)
;Dandu-se o lista, sa se construiasca lista primelor elemente ale tuturor elementelor
;lista ce au un numar impar de elemente la nivel superficial. 
;Exemplu: (1 2 (3 (4 5) (6 7)) 8 (9 10 11)) => (1 3 9). 
(defun nratomi(L)
	(cond
		((null L) 0)
		((atom (car L)) (+ 1 (nratomi (cdr L))))
		(T (nratomi (cdr L)))
	)
)

(defun nratomiimpar(L) (= 1(mod (nratomi L) 2)))

(defun prat(L)
	(cond
		((null L) NIL)
		((atom (car L)) (car L))
		(T (prat (cdr L)))
	)
)

(defun sub_a(L)
	(cond
		((null L) NIL)
		((atom (car L)) (sub_a (cdr L)))
		(T (append (list (car L)) (sub_a (car L)) (sub_a (cdr L))))
	)
)

(defun submultimi(L) (append (list L) (sub_a L)))

(defun restrange_a(L)
	(cond
		((null L) nil)
		((nratomiimpar (car L)) (cons (prat (car L)) (restrange_a (cdr L))))
		(T (restrange_a (cdr L)))
	)
)

(defun restrange(L) (restrange_a (submultimi L)))

;Sa se scrie o functie care, primind o lista, intoarce multimea tuturor perechilor 
;din lista. De exemplu: (a b c d) --> ((a b) (a c) (a d)(b c) (b d) (c d)) 
(defun pereche(L A)
	(cond
		((null L) NIL)
		(T (cons (list A (car L)) (pereche (cdr L) A)))
	)
)

(defun perechi(L)
	(cond
		((null L) NIL)
		(T (append (pereche (cdr L) (car L)) (perechi (cdr L))))
	)
)

;permutari
(defun inser_poz(L A P)
	(cond
		((and(null L)(= P 1))(list A))
		((null L) NIL)
		((= P 1) (cons A L))
		(T (cons (car L) (inser_poz (cdr L) A (- P 1))))
	)
)


(defun insereaza(L A K)
	(cond
		((= K 0) NIL)
		(T (cons (inser_poz L A K)(insereaza L A (- K 1))))
	)
)

(defun insereaza_liste(L A)
	(cond
		((null L) NIL)
		(T (append (insereaza (car L) A (+ 1(length (car L)))) (insereaza_liste (cdr L) A)))
	)
)

(defun permutari(L)
	(cond
		((null L)NIL)
		((null (cdr L)) (list (list (car L))))
		(T (insereaza_liste (permutari (cdr L)) (car L)))
	)
)