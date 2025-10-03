(defsystem "normal"
  :version "0.0.1"
  :author "nmunro"
  :license "BSD3-Clause"
  :depends-on (:closer-mop
               :cl-dbi
               :local-time)
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "Generate a skeleton for modern project"
  :in-order-to ((test-op (test-op "normal/tests"))))

(defsystem "normal/tests"
  :author "nmunro"
  :license "BSD3-Clause"
  :depends-on ("normal"
               :rove)
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for normal"
  :perform (test-op (op c) (symbol-call :rove :run c)))
