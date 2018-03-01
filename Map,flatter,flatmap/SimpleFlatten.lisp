(defun myFlatten (l) (if( not (eq (car (cdr (cdr l))) nil)) l (myFlatten (append (car l) (car (reverse l))))))
