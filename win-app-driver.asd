;;;; MIT License
;;;; 
;;;; Copyright (c) 2022 kzO8Xekb
;;;; 
;;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;;; of this software and associated documentation files (the "Software"), to deal
;;;; in the Software without restriction, including without limitation the rights
;;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;;; copies of the Software, and to permit persons to whom the Software is
;;;; furnished to do so, subject to the following conditions:
;;;; 
;;;; The above copyright notice and this permission notice shall be included in all
;;;; copies or substantial portions of the Software.
;;;; 
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;;; SOFTWARE.

(defsystem "win-app-driver"
           :version "0.1.2"
           :author "kzO8Xekb"
           :license "MIT"
           :depends-on ("cl-ppcre"
                        "dexador"
                        "jonathan"
                        "let-over-lambda"
                        "local-time"
                        "trivial-macroexpand-all")
           :components ((:module "src"
                                 :components
                                 ((:file "package")
                                  (:module "apis"
                                           :depends-on ("package" "conditions" "utilities")
                                           :serial t
                                           :components
                                           ((:file "client")
                                            (:file "apis")
                                            (:file "main")))
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
           :version "0.1.2"
           :author "kzO8Xekb"
           :license "MIT"
           :depends-on ("win-app-driver"
                        "prove"
                        "quri"
                        "uiop")
           :components ((:module "tests"
                                 :serial t
                                 :components
                                 ((:file "package")
                                  (:file "config")
                                  (:file "client-test")
                                  (:file "main-test")
                                  (:file "apis-test")
                                  (:file "finalizer"))))
           :description "This package is WinAppDriver utilities test suite."
           :perform (test-op (op c) (symbol-call :prove :run c)))

