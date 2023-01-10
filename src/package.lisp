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

(defpackage win-app-driver
  (:use :cl :lol)
  (:shadow :condition-incorrect-arguments)

  (:import-from :dexador)
  (:import-from :jonathan)
  (:import-from :let-over-lambda)
  (:import-from :local-time)
  (:import-from :trivial-macroexpand-all)
  (:import-from :uiop)
  
  ; conditions/conditions.lisp
  (:export
    :condition-incorrect-arguments
    :condition-incorrect-hostname-string
    :condition-incorrect-mouse-button
    :condition-incorrect-port-number
    :condition-unexpected-error
    :condition-unknown-http-command-error
    :define-conditions
    :finish
    :make-condition-if
    :make-error-condition-if
    :make-signal-condition-if
    :make-warning-condition-if
    :progn-on-preconditions
    :progn-to-postconditions
    :progn-with-recovery-conditions
    :try-again
    :win-app-driver-error-condition
    :win-app-driver-simple-condition
    :win-app-driver-warning-condition
    )

  ; apis/main.lisp
  (:export
    :create-session
    :self
    :session-data
    :session-data-base
    :session-data-capabilities
    :session-data-host
    :session-data-id
    :session-data-port
    )

  (:export
    :mouse-left-button
    :mouse-middle-button
    :mouse-right-button
    )

  ; apis/element.lisp
  (:export
    :get-element-id)

  ; utilities/util.lisp
  (:export
    :it
    :response-accessor
    :get-value)
  )

