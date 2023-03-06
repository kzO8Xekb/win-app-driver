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

; Take Screenshot
; HTTP Command:  GET
; Path:         /session/:sessionId/screenshot
(defun take-screenshot (session)
  "take-screenshot
  _/_/_/ summary _/_/_/
  This function take a screenshot of the application.
  If the function is successful, the \"value\" property of the JSON-encoded WinAppServer response contains image data in png format encoded as text data in Base64 format.
  The image can be viewed by decoding it using a package such as cl-base64.

  _/_/_/ argument _/_/_/
  session, session-data, Specifies the session with WinAppDriver.

  _/_/_/ return value _/_/_/
  STRING,                 Response from WinAppDriver Server. JSON format text data.
  INTEGER,                HTTP Server response code.
  HASHTABLE,              One of the return values from dexador. Usage unknown.
  QURI.URI.HTTP:URI-HTTP, Structure representing the query URI."
  (send-command
    session
    :get
    ((session-data-base session) "/screenshot")))

; Take Element Screenshot
; HTTP Command: GET
; Path:         /session/:sessionId/element/:id/screenshot
(defun take-element-screenshot (session element-id)
  (send-command
    session
    :get
    ((session-data-base session) "/element/" element-id "/screenshot")))

