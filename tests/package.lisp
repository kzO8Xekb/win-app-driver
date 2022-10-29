(defpackage win-app-driver/tests
  (:use :cl
        :win-app-driver
        :prove))

(in-package :win-app-driver/tests)

;; NOTE: To run this test file, execute `(asdf:test-system :win-app-driver)' in your Lisp.

(setf prove:*enable-colors* nil)

(plan nil)

