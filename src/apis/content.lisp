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

(in-package :win-app-driver)

(declaim (inline generate-content-of-position))
(defun generate-content-of-position (x y)
  (jonathan:to-json `(:|x| ,x :|y| ,y)))

(declaim (inline generate-content-of-window-size))
(defun generate-content-of-window-size (width height)
  (jonathan:to-json `(:|height| ,height :|width| ,width)))

(declaim (inline generate-content-of-touch-element))
(defun generate-content-of-touch-element (element-id)
  (jonathan:to-json
    `(:|element| ,element-id)))

(declaim (inline generate-mouse-button-content))
(defun generate-mouse-button-content (button)
  (case button
    (:mouse-left-button   (jonathan:to-json `(:|button| 0)))
    (:mouse-middle-button (jonathan:to-json `(:|button| 1)))
    (:mouse-right-button  (jonathan:to-json `(:|button| 2)))
    (otherwise            (error
                            (make-condition
                              'win-app-driver:condition-incorrect-mouse-button)))))

