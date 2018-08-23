;;;; src/trivial-sat-solver.lisp

(in-package #:trivial-sat-solver)

(defun const? (atm)
  (and (atom atm) (member atm '(true false))))

(defun var? (atm)
  (and (atom atm) (not (member atm '(and or not true false)))))

(defun invert (atm)
  (if (eq 'true atm) 'false 'true))

(defun free-variables-in (tree)
  (labels ((rec (tree)
	     (cond ((var? tree) (list tree))
		   ((atom tree) nil)
		   (t (loop for elem in tree
			 append (rec elem))))))
    (remove-duplicates (rec tree))))

(defun assign (var val tree)
  (subst val var tree))

(defun simplify (tree)
  (if (atom tree)
      tree
      (let ((simple (mapcar #'simplify tree)))
	(cond ((eq 'not (car simple))
	       (if (const? (cadr simple))
		   (invert (cadr simple))
		   simple))

	      ((eq 'or (car simple))
	       (let ((s (remove 'false (rest simple))))
		 (cond ((null s) 'false)
		       ((not (cdr s)) (car s))
		       ((find 'true s) 'true)
		       (t (cons 'or s)))))

	      ((eq 'and (car simple))
	       (let ((s (remove 'true (rest simple))))
		 (cond ((null s) 'true)
		       ((not (cdr s)) (car s))
		       ((find 'false s) 'false)
		       (t (cons 'and s)))))))))

(defun satisfiable? (tree)
  (let ((vars (free-variables-in tree)))
    (if (null vars)
	(eq 'true (simplify tree))
	(or (satisfiable? (assign (first vars) 'true tree))
	    (satisfiable? (assign (first vars) 'false tree))))))

(defun satisfiable-by (tree &optional bindings)
  (let ((vars (free-variables-in tree)))
    (if (null vars)
	(when (eq 'true (simplify tree)) bindings)
	(let ((v (first vars)))
	  (or (satisfiable-by
	       (assign v 'true tree)
	       (cons (cons v 'true) bindings))
	      (satisfiable-by
	       (assign v 'false tree)
	       (cons (cons v 'false) bindings)))))))
