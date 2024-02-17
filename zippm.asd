(defsystem "zippm"
  :version "0.1.0"
  :author "Daniel Jay Haskin"
  :license "MIT"
  :depends-on (
               "cl-i"
               "alexandria"
  )
  :components ((:module "src"
                :components
                ((:file "main")
                 (:file "version"))))
  :description ""
  :in-order-to ((test-op (test-op "zippm/tests"))))

(defsystem "zippm/tests"
  :author "Daniel Jay Haskin"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("zippm"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main")
                 (:file "version"))))
  :description "Test system for zippm"
  :perform (test-op (op c) (symbol-call :rove :run c)))
