(defun substituie(L e1 e2)
    (cond
        ((and (atom L) (equal L e1)) e2)
        ((atom L) L)
        (T (mapcar #'(lambda(L)
                        (substituie L e1 e2)
                     )
                     L
            )
        )
    )
)