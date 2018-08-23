;;;; trivial-sat-solver.asd

(asdf:defsystem #:trivial-sat-solver
  :description "Describe trivial-sat-solver here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :version "0.0.1"
  :serial t
  :components ((:module
		src :components
		((:file "package")
		 (:file "trivial-sat-solver")))))

(asdf:defsystem #:trivial-sat-solver-test
  :description "Test suite for :trivial-sat-solver"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:trivial-sat-solver #:test-utils)
  :defsystem-depends-on (#:prove-asdf)
  :components ((:module
                test :components
                ((:file "package")
                 (:test-file "trivial-sat-solver"))))
  :perform (test-op
	    :after (op c)
	    (funcall (intern #.(string :run) :prove) c)))
