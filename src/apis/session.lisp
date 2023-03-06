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

(defun get-msex-wait-for-app-launch (capabilities)
  (aif (getf                                                   ; Get the value of ms:waitForAppLaunch from desired capabilities.
         (getf 
           (jonathan:parse capabilities)
           :|desiredCapabilities|)
         :|ms:waitForAppLaunch|)
       (+ (read-from-string it)                                ; Convert to numerical values and add margins.
          dex:*default-read-timeout*)                          ; Increase the read time, as it may time out if the default value is used.
       dex:*default-read-timeout*))                            ; default value.

; New Session
; HTTP Command: POST
; Path:         /session
; The new-session command create a new Windows Application Driver session with the endpoint node.
(defun new-session (session)
  "The new-session command create a new Windows Application Driver session with the endpoint node."
  (send-command
    session
    :post
    ("/session")
    (session-data-capabilities session)
    (get-msex-wait-for-app-launch                              ; for WinAppDriver v1.2.1
      (session-data-capabilities session))))

; Get Sessions
; HTTP Command: GET
; Path:         /sessions
; This function is not yet implemented.
(defun get-sessions (session)
  (send-command
    session
    :get
    ("/sessions")))

; Delete Session
; HTTP Command: DELETE
; Path:         /session/:sessionId
(defun delete-session (session)
  (send-command
    session
    :delete
    ((session-data-base session))))

