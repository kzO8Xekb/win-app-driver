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

(defparameter +empty-data+               "{\"\": \"\"}")

;; Command Summary
;; see https://github.com/microsoft/WinAppDriver/blob/master/Docs/SupportedAPIs.md
; The following is a list of APIs supported by WinAppDriver:
; HTTP 	Path

; Status
; GET /status
(defun status (session)
  (send-command
    session
    :get
    ("/status")))

; New Session
; HTTP Method: POST
; URI Template: /session
; The new-session command create a new Windows Application Driver session with the endpoint node.
(defun new-session (session)
  (send-command
    session
    :post
    ("/session")
    (session-data-capabilities session)))

; Get Sessions
; HTTP Method: GET
; URI Template: /sessions
; 一応の実装。
; 原則として，クロージャから呼び出さない。
; セッションとクロージャーは1対1に対応させた方が良いと思ったため。
(defun get-sessions (session)
  (send-command
    session
    :get
    ("/sessions")))

; Delete Session
; HTTP Method: DELETE
; URI Template: /session/:sessionId
(defun delete-session (session)
  (send-command
    session
    :delete
    ((session-data-base session))))

;POST 	/session/:sessionId/appium/app/launch
;POST 	/session/:sessionId/appium/app/close
;POST 	/session/:sessionId/back
;POST 	/session/:sessionId/buttondown
;POST 	/session/:sessionId/buttonup
;POST 	/session/:sessionId/click
;POST 	/session/:sessionId/doubleclick
;POST 	/session/:sessionId/element
;POST 	/session/:sessionId/elements
;POST 	/session/:sessionId/element/active
;GET 	/session/:sessionId/element/:id/attribute/:name
;POST 	/session/:sessionId/element/:id/clear
;POST 	/session/:sessionId/element/:id/click
;GET 	/session/:sessionId/element/:id/displayed
;GET 	/session/:sessionId/element/:id/element
;GET 	/session/:sessionId/element/:id/elements
;GET 	/session/:sessionId/element/:id/enabled
;GET 	/session/:sessionId/element/:id/equals
;GET 	/session/:sessionId/element/:id/location
;GET 	/session/:sessionId/element/:id/location_in_view
;GET 	/session/:sessionId/element/:id/name
;GET 	/session/:sessionId/element/:id/screenshot
;GET 	/session/:sessionId/element/:id/selected
;GET 	/session/:sessionId/element/:id/size
;GET 	/session/:sessionId/element/:id/text
;POST 	/session/:sessionId/element/:id/value
;POST 	/session/:sessionId/forward
;POST 	/session/:sessionId/keys
;GET 	/session/:sessionId/location
;POST 	/session/:sessionId/moveto
;GET 	/session/:sessionId/orientation
;GET 	/session/:sessionId/screenshot
;GET 	/session/:sessionId/source
;POST 	/session/:sessionId/timeouts
;GET 	/session/:sessionId/title
;POST 	/session/:sessionId/touch/click
;POST 	/session/:sessionId/touch/doubleclick
;POST 	/session/:sessionId/touch/down
;POST 	/session/:sessionId/touch/flick
;POST 	/session/:sessionId/touch/longclick
;POST 	/session/:sessionId/touch/move
;POST 	/session/:sessionId/touch/scroll
;POST 	/session/:sessionId/touch/up
;DELETE 	/session/:sessionId/window
;POST 	/session/:sessionId/window
;POST 	/session/:sessionId/window/maximize
;POST 	/session/:sessionId/window/size
;GET 	/session/:sessionId/window/size
;POST 	/session/:sessionId/window/:windowHandle/size
;GET 	/session/:sessionId/window/:windowHandle/size
;POST 	/session/:sessionId/window/:windowHandle/position
;GET 	/session/:sessionId/window/:windowHandle/position
;POST 	/session/:sessionId/window/:windowHandle/maximize
;GET 	/session/:sessionId/window_handle
;GET 	/session/:sessionId/window_handles

