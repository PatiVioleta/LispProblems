(defun lungime(l)
	(cond
		((null L) 0)
		(T (+ 1 (lungime (cdr l))))
	)
)

(defun lgm(l)
	(cond
		((atom l) 0)
		(T (max (lungime l)(apply #'max (mapcar #'lgm l))))
	)
)

(defun elimin(l)
	(cond
		((numberp l) NIL)
		((atom l) (list l))
		(T (list (apply #'append (mapcar #'elimin l))))
	)
)

(defun m (L)
       (cond
	   ((numberp L) L)  
	   ((atom L) most-negative-fixnum)
	   (t (apply #'max  (mapcar #'m L)))
	   )
) 
 
(defun lista (L)
       (mapcan #'(lambda (v)    
					(cond      
						((= 0 (mod v 2) (list v)))
						(t nil)
					) (m L)
				   )
	            L)
	   )
