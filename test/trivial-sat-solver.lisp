;;;; test/trivial-sat-solver.lisp

(in-package #:trivial-sat-solver-test)

(tests
 (is (+ 2 3) 5 "Addition works")
 (is (+ 2 3) 6 "Intentionally fails")

 (for-all ((a a-number) (b a-number))
	  (is= (+ a b) (+ b a))
	  "Addition is commutative"))
