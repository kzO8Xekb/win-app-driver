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

(in-package :win-app-driver/tests)

(let
  ((calculator-session (win-app-driver::create-session)))
  (funcall                                                ; Run Calcurator
    calculator-session
    :new-session
    :host *win-app-driver-host*
    :port *win-app-driver-port*
    :app "Microsoft.WindowsCalculator_8wekyb3d8bbwe!App")
  (flet
    ((get-axis-point (element-identifier)
                     (let*
                       ((elemenet-id (win-app-driver::get-element-id
                                       (funcall calculator-session
                                                :find-element
                                                :automation-id element-identifier)))
                        (response    (funcall calculator-session
                                              :get-element-location
                                              elemenet-id)))
                       (values
                         elemenet-id
                         (getf (win-app-driver::get-value response) :|x|)
                         (getf (win-app-driver::get-value response) :|y|)))))
    (let
      ((base (win-app-driver::session-data-base
                        (funcall
                          calculator-session
                          :pandoric-get
                          'win-app-driver::session)))
       clear-button-id
       clear-button-x
       clear-button-y
       equal-button-id
       equal-button-x
       equal-button-y
       num1-button-id
       num1-button-x
       num1-button-y
       plus-button-id
       plus-button-x
       plus-button-y)
      (setf
        (values num1-button-id num1-button-x num1-button-y)    (get-axis-point "num1Button")
        (values plus-button-id plus-button-x plus-button-y)    (get-axis-point "plusButton")
        (values equal-button-id equal-button-x equal-button-y) (get-axis-point "equalButton")
        (values clear-button-id clear-button-x clear-button-y) (get-axis-point "clearButton"))
      ;; mouse operations

      ;; touch operations
      (subtest "Testing touch-click."
               (test-api
                 (funcall
                   calculator-session
                   :touch-click
                   num1-button-id)
                 :content-length "63"
                 :path           (concatenate
                                   'string
                                   base
                                   "/touch/click")))

      (funcall calculator-session :touch-click plus-button-id)

      (subtest "Testing touch-double-click."
               (test-api
                 (funcall
                   calculator-session
                   :touch-double-click
                   num1-button-id)
                 :content-length "63"
                 :path           (concatenate
                                   'string
                                   base
                                   "/touch/doubleclick")))

      (subtest "Testing touch-long-click."
               (test-api
                 (funcall
                   calculator-session
                   :touch-long-click
                   equal-button-id)
                 :content-length "63"
                 :path           (concatenate
                                   'string
                                   base
                                   "/touch/longclick")))

      (subtest "Testing touch-down."
               (test-api
                 (funcall
                   calculator-session
                   :touch-down
                   clear-button-x
                   clear-button-y)
                 :content-length "63"
                 :path           (concatenate
                                   'string
                                   base
                                   "/touch/down")))

      (subtest "Testing touch-up."
               (test-api
                 (funcall
                   calculator-session
                   :touch-up
                   clear-button-x
                   clear-button-y)
                 :content-length "63"
                 :path           (concatenate
                                   'string
                                   base
                                   "/touch/up")))

      (subtest "Testing touch-flick."
               (test-api
                 (funcall
                   calculator-session
                   :touch-flick
                   -50
                   50)
                 :content-length "63"
                 :path           (concatenate
                                   'string
                                   base
                                   "/touch/flick")))

      (funcall calculator-session :touch-flick  50 -50)

      (subtest "Testing touch-move."
               (test-api
                 (funcall
                   calculator-session
                   :touch-move
                   -50
                   50)
                 :content-length "63"
                 :path           (concatenate
                                   'string
                                   base
                                   "/touch/move")))

      (funcall calculator-session :touch-move   50 -50)

      (subtest "Testing touch-scroll."
               (test-api
                 (funcall calculator-session
                          :touch-scroll
                          num1-button-id
                          0
                          50)
                 :content-length "63"
                 :path           (concatenate
                                   'string
                                   base
                                   "/touch/scroll")))

      (funcall calculator-session :touch-scroll num1-button-id 0 -50)

      (funcall
        calculator-session
        :delete-session))))

