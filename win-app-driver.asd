(defsystem "win-app-driver"
  :version "0.0.1"
  :author "kzO8Xekb"
  :license "MIT"
  :depends-on ("let-over-lambda"
               "dexador"
               "jonathan"
               "uiop"
               "trivial-macroexpand-all")
  :components ((:module "src"
                :components
                ((:file "package")
                 (:module "apis"
                               :depends-on ("package" "utilities")
                               :components
                               ((:file "win-app-driver")))
                 (:module "utilities"
                               :depends-on ("package")
                               :components
                               ((:file "util"))))))
  :in-order-to ((test-op (test-op "win-app-driver/tests"))))

(defsystem "win-app-driver/tests"
  :version "0.0.1"
  :author "kzO8Xekb"
  :license ""
  :depends-on ("win-app-driver"
               "prove")
  :components ((:module "tests"
                :components
                ((:file "test"))))
  :description "This package is WinAppDriver utilities test suite."
  :perform (test-op (op c) (symbol-call :prove :run c)))

