(defsystem "win-app-driver"
           :version "0.0.3"
           :author "kzO8Xekb"
           :license "MIT"
           :depends-on ("cl-ppcre"
                        "dexador"
                        "jonathan"
                        "let-over-lambda"
                        "local-time"
                        "trivial-macroexpand-all"
                        "uiop")
           :components ((:module "src"
                                 :components
                                 ((:file "package")
                                  (:module "apis"
                                           :depends-on ("package" "conditions" "utilities")
                                           :components
                                           ((:file "win-app-driver")))
                                  (:module "conditions"
                                           :depends-on ("package" "utilities")
                                           :components
                                           ((:file "conditions")))
                                  (:module "utilities"
                                           :depends-on ("package")
                                           :components
                                           ((:file "util"))))))
           :in-order-to ((test-op (test-op "win-app-driver/tests"))))

(defsystem "win-app-driver/tests"
           :version "0.0.3"
           :author "kzO8Xekb"
           :license "MIT"
           :depends-on ("win-app-driver"
                        "prove")
           :components ((:module "tests"
                                 :components
                                 ((:file "package")
                                  (:file "ipv6-test")
                                  (:file "apis-test"))))
           :description "This package is WinAppDriver utilities test suite."
           :perform (test-op (op c) (symbol-call :prove :run c)))

