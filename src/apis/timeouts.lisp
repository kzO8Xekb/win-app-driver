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

;; Command Summary
;; see https://github.com/microsoft/WinAppDriver/blob/master/Docs/SupportedAPIs.md
; The following is a list of APIs supported by WinAppDriver:
; HTTP 	Path

; In the case of webdriver, there are three types of timeouts: "script", "pageLoad", and "implicit".
; see https://www.w3.org/TR/webdriver/#dfn-timeouts-configuration
;
; For now, only "implicit" is supported.
(declaim (inline generate-content-of-timeouts))
(defun generate-content-of-timeouts (&key implicit)
  (jonathan:to-json `(:|implicit| ,implicit)))

; Set Timeouts
; HTTP Command:  POST
; Path:         /session/:sessionId/timeouts
(defun set-timeouts (session &key implicit)
  (send-command
    session
    :post
    ((session-data-base session) "/timeouts")
    (generate-content-of-timeouts
      :implicit implicit)))

