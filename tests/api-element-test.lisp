;;;; MIT License
;;;; 
;;;; Copyright (c) 2016 Microsoft
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

;; NOTE: To run this test file, execute `(asdf:test-system :win-app-driver)' in your Lisp.
(subtest "Testing locator-strategies."
         (is-values
           (win-app-driver::locator-strategies :accessibility-id-selector)
           '("accessibility id" t))
         (is-values
           (win-app-driver::locator-strategies :accessibility-id)
           '("accessibility id" t))
         (is-values
           (win-app-driver::locator-strategies :automation-id-selector)
           '("accessibility id" t))
         (is-values
           (win-app-driver::locator-strategies :automation-id)
           '("accessibility id" t))
         (is-values
           (win-app-driver::locator-strategies :class-name-selector)
           '("class name" t))
         (is-values
           (win-app-driver::locator-strategies :class-name)
           '("class name" t))
         (is-values
           (win-app-driver::locator-strategies :name-selector)
           '("name" t))
         (is-values
           (win-app-driver::locator-strategies :name)
           '("name" t))
         (is-values
           (win-app-driver::locator-strategies :xpath-selector)
           '("xpath" t))
         (is-values
           (win-app-driver::locator-strategies :xpath)
           '("xpath" t))
         (is-values
           (win-app-driver::locator-strategies :foo)
           '(nil nil))
         )

(subtest "Testing generate-element-string."
         (is
           (win-app-driver::generate-element-string "accessibility id" "foo")
           "{\"using\":\"accessibility id\",\"value\":\"foo\"}")
         (is
           (win-app-driver::generate-element-string "class name" "foo")
           "{\"using\":\"class name\",\"value\":\"foo\"}")
         (is
           (win-app-driver::generate-element-string "name" "foo")
           "{\"using\":\"name\",\"value\":\"foo\"}")
         (is
           (win-app-driver::generate-element-string "xpath" "foo")
           "{\"using\":\"xpath\",\"value\":\"foo\"}")
         )

(subtest "Testing generate-find-element-json-string."
         (is
           (win-app-driver::generate-find-element-json-string :accessibility-id-selector "foo")
           "{\"using\":\"accessibility id\",\"value\":\"foo\"}")
         (is
           (win-app-driver::generate-find-element-json-string :accessibility-id "foo")
           "{\"using\":\"accessibility id\",\"value\":\"foo\"}")
         (is
           (win-app-driver::generate-find-element-json-string :automation-id-selector "foo")
           "{\"using\":\"accessibility id\",\"value\":\"foo\"}")
         (is
           (win-app-driver::generate-find-element-json-string :automation-id "foo")
           "{\"using\":\"accessibility id\",\"value\":\"foo\"}")
         (is
           (win-app-driver::generate-find-element-json-string :class-name-selector "foo")
           "{\"using\":\"class name\",\"value\":\"foo\"}")
         (is
           (win-app-driver::generate-find-element-json-string :class-name "foo")
           "{\"using\":\"class name\",\"value\":\"foo\"}")
         (is
           (win-app-driver::generate-find-element-json-string :name-selector "foo")
           "{\"using\":\"name\",\"value\":\"foo\"}")
         (is
           (win-app-driver::generate-find-element-json-string :name "foo")
           "{\"using\":\"name\",\"value\":\"foo\"}")
         (is
           (win-app-driver::generate-find-element-json-string :xpath-selector "foo")
           "{\"using\":\"xpath\",\"value\":\"foo\"}")
         (is
           (win-app-driver::generate-find-element-json-string :xpath "foo")
           "{\"using\":\"xpath\",\"value\":\"foo\"}")
         (is
           (win-app-driver::generate-find-element-json-string :xpath "")
           "{\"using\":\"xpath\",\"value\":\"\"}")
         (is-error
           (win-app-driver::generate-find-element-json-string :xpath)
           sb-int:simple-program-error)
         (is-error
           (win-app-driver::generate-find-element-json-string :foo "foo")
           win-app-driver::condition-incorrect-arguments)
         )

