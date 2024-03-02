(defsystem "skin.djha.zippm"
	   :version "0.1.0"
	   :author "Daniel Jay Haskin"
	   :license "MIT"
	   :depends-on ("cl-i"
			"esrap"
			"cl-semver"
			"alexandria")
	   :components ((:module "src"
				 :components
				 ((:file "main")
				  (:file "resolve"))))
	   :description ""
	   :in-order-to ((asdf:test-op (asdf:test-op "skin.djha.zippm/tests"))))

(defsystem "skin.djha.zippm/tests"
	   :author "Daniel Jay Haskin"
	   :license "MIT"
	   :version "0.1.0"
	   :depends-on ("skin.djha.zippm"
			"parachute")
	   :components ((:module "tests"
				 :components
				 ((:file "main")
				  (:file "resolve"))))
	   :description "Test system for zippm"
	   :perform (asdf:test-op (op c)
				  (uiop:symbol-call
				    :parachute
				    :test :skin.djha.zippm/tests)
				  (uiop:symbol-call
				    :parachute
				    :test :skin.djha.zippm/tests/resolve)))
