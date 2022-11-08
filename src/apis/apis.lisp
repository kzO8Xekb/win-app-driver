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

(defparameter +empty-data+ "{\"\": \"\"}")

;; Command Summary
;; see https://github.com/microsoft/WinAppDriver/blob/master/Docs/SupportedAPIs.md
; The following is a list of APIs supported by WinAppDriver:
; HTTP 	Path

; Status
; GET /status
; WinAppDriver Serverのステータスを取得する。
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
(defun launch-app (session)
  (send-command
    session
    :post
    ((session-data-base session) "/appium/app/launch")))

;POST 	/session/:sessionId/appium/app/close
(defun close-app (session)
  (send-command
    session
    :post
    ((session-data-base session) "/appium/app/close")))

;POST 	/session/:sessionId/back
(defun back (session)
  (send-command
    session
    :post
    ((session-data-base session) "/back")))

;POST 	/session/:sessionId/buttondown
(defun button-down (session)
  (send-command
    session
    :post
    ((session-data-base session) "/buttondown")))

;POST 	/session/:sessionId/buttonup
(defun button-up (session)
  (send-command
    session
    :post
    ((session-data-base session) "/buttonup")))

;POST 	/session/:sessionId/click
(defun click (session)
  (send-command
    session
    :post
    ((session-data-base session) "/click")))

;POST 	/session/:sessionId/doubleclick
(defun doubleclick (session)
  (send-command
    session
    :post
    ((session-data-base session) "/doubleclick")))

;POST 	/session/:sessionId/element
(defun find-element (session)
  (send-command
    session
    :post
    ((session-data-base session) "/element")))

;POST 	/session/:sessionId/elements
(defun find-elements (session) 
  (send-command
    session
    :post
    ((session-data-base session) "/elements")))

;POST 	/session/:sessionId/element/active
(defun active-element (session)
  (send-command
    session
    :post
    ((session-data-base session) "/element/active")))

;GET 	/session/:sessionId/element/:id/attribute/:name
(defun get-element-attribute (session) nil)

;POST 	/session/:sessionId/element/:id/clear
(defun element-clear (session) nil)

;POST 	/session/:sessionId/element/:id/click
(defun element-click (session) nil)

;GET 	/session/:sessionId/element/:id/displayed
(defun element-displayed (session) nil)

;GET 	/session/:sessionId/element/:id/element
(defun find-element-from-element (session) nil)

;GET 	/session/:sessionId/element/:id/elements
(defun find-element-from-elements (session) nil)

;GET 	/session/:sessionId/element/:id/enabled
(defun is-element-enabled (session) nil)

;GET 	/session/:sessionId/element/:id/equals
(defun element-equals (session) nil)

;GET 	/session/:sessionId/element/:id/location
(defun get-element-location (session) nil)

;GET 	/session/:sessionId/element/:id/location_in_view
(defun get-element-location-in-view (session) nil)

;GET 	/session/:sessionId/element/:id/name
(defun get-element-name (session) nil)

;GET 	/session/:sessionId/element/:id/screenshot
(defun take-element-screenshot (session) nil)

;GET 	/session/:sessionId/element/:id/selected
(defun is-element-selected (session) nil)

;GET 	/session/:sessionId/element/:id/size
(defun get-element-size (session) nil)

;GET 	/session/:sessionId/element/:id/text
(defun get-element-text (session) nil)

;POST 	/session/:sessionId/element/:id/value
(defun get-element-value (session) nil)

;POST 	/session/:sessionId/forward
(defun forward (session) nil)

;POST 	/session/:sessionId/keys
(defun send-keys (session) nil)

;GET 	/session/:sessionId/location
(defun location (session) nil)

;POST 	/session/:sessionId/moveto
(defun move-to (session) nil)

;GET 	/session/:sessionId/orientation
(defun orientation (session) nil)

;GET 	/session/:sessionId/screenshot
(defun take-screenshot (session) nil)

;GET 	/session/:sessionId/source
(defun get-source (session) nil)

;POST 	/session/:sessionId/timeouts
(defun set-timeouts (session) nil)

;GET 	/session/:sessionId/title
(defun get-title (session) nil)

;POST 	/session/:sessionId/touch/click
(defun touch-click (session) nil)

;POST 	/session/:sessionId/touch/doubleclick
(defun touch-doubleclick (session) nil)

;POST 	/session/:sessionId/touch/down
(defun touch-down (session) nil)

;POST 	/session/:sessionId/touch/flick
(defun touch-flick (session) nil)

;POST 	/session/:sessionId/touch/longclick
(defun touch-longclick (session) nil)

;POST 	/session/:sessionId/touch/move
(defun touch-move (session) nil)

;POST 	/session/:sessionId/touch/scroll
(defun touch-scroll (session) nil)

;POST 	/session/:sessionId/touch/up
(defun touch-up (session) nil)

;DELETE 	/session/:sessionId/window
(defun close-window (session) nil)

;POST 	/session/:sessionId/window
(defun switch-to-window (session) nil)

;POST 	/session/:sessionId/window/maximize
(defun maximize-window (session) nil)

;POST 	/session/:sessionId/window/size
(defun get-window-size (session) nil)

;GET 	/session/:sessionId/window/size
(defun set-window-size (session) nil)

;POST 	/session/:sessionId/window/:windowHandle/size
(defun get-window-size-with-window-handle (session) nil)

;GET 	/session/:sessionId/window/:windowHandle/size
(defun set-window-size-with-window-handle (session) nil)

;POST 	/session/:sessionId/window/:windowHandle/position
(defun get-window-position-with-window-handle (session) nil)

;GET 	/session/:sessionId/window/:windowHandle/position
(defun set-window-position-with-window-handle (session) nil)

;POST 	/session/:sessionId/window/:windowHandle/maximize
(defun window-maximize-with-window-handle (session) nil)

;GET 	/session/:sessionId/window_handle
(defun get-window-handle (session) nil)

;GET 	/session/:sessionId/window_handles
(defun get-window-handles (session) nil)

