(defun flatten (l)
  	(if (endp l)
      	l
	    (if (atom (car l ))
	    	(append (list (car l)) (flatten (cdr l)))
	      	(append (flatten (car l)) (flatten (cdr l )))
      	)
    )
)
