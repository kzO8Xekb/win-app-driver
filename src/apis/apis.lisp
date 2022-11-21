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

;(defparameter +empty-data+ "{\"\": \"\"}")

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
(defun find-element (session selector value)
  (send-command
    session
    :post
    ((session-data-base session) "/element")))

;POST 	/session/:sessionId/elements
(defun find-elements (session selector value) 
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
(defun get-element-attribute(session element-id attribute-name)
  (send-command
    session
    :get
    ((session-data-base session) "/element/" element-id "/attribute/" attribute-name)))

;POST 	/session/:sessionId/element/:id/clear
(defun element-clear(session element-id)
  (send-command
    session
    :post
    ((session-data-base session) "/element/" element-id "/clear")))

;POST 	/session/:sessionId/element/:id/click
(defun element-click(session element-id)
  (send-command
    session
    :post
    ((session-data-base session) "/element/" element-id "/click")))

;GET 	/session/:sessionId/element/:id/displayed
(defun is-element-displayed(session element-id)
  (send-command
    session
    :get
    ((session-data-base session) "/element/" element-id "/displayed")))

;GET 	/session/:sessionId/element/:id/element
(defun find-element-from-element(session element-id selector value)
  (send-command
    session
    :get
    ((session-data-base session) "/element/" element-id "/element")))

;GET 	/session/:sessionId/element/:id/elements
(defun find-element-from-elements(session element-id selector value)
  (send-command
    session
    :get
    ((session-data-base session) "/element/" element-id "/elements")))

;GET 	/session/:sessionId/element/:id/enabled
(defun is-element-enabled(session element-id)
  (send-command
    session
    :get
    ((session-data-base session) "/element/" element-id "/enabled")))

;GET 	/session/:sessionId/element/:id/equals
(defun element-equals(session element-id)
  (send-command
    session
    :get
    ((session-data-base session) "/element/" element-id "/equals")))

;GET 	/session/:sessionId/element/:id/location
(defun get-element-location(session element-id)
  (send-command
    session
    :get
    ((session-data-base session) "/element/" element-id "/location")))

;GET 	/session/:sessionId/element/:id/location_in_view
(defun get-element-location-in-view(session element-id)
  (send-command
    session
    :get
    ((session-data-base session) "/element/" element-id "/location_in_view")))

;GET 	/session/:sessionId/element/:id/name
(defun get-element-name(session element-id)
  (send-command
    session
    :get
    ((session-data-base session) "/element/" element-id "/name")))

;GET 	/session/:sessionId/element/:id/screenshot
(defun take-element-screenshot(session element-id)
  (send-command
    session
    :get
    ((session-data-base session) "/element/" element-id "/screenshot")))

;GET 	/session/:sessionId/element/:id/selected
(defun is-element-selected(session element-id)
  (send-command
    session
    :get
    ((session-data-base session) "/element/" element-id "/selected")))

;GET 	/session/:sessionId/element/:id/size
(defun get-element-size(session element-id)
  (send-command
    session
    :get
    ((session-data-base session) "/element/" element-id "/size")))

;GET 	/session/:sessionId/element/:id/text
(defun get-element-text(session element-id)
  (send-command
    session
    :get
    ((session-data-base session) "/element/" element-id "/text")))

;POST 	/session/:sessionId/element/:id/value
(defun get-element-value(session element-id)
  (send-command
    session
    :post
    ((session-data-base session) "/element/" element-id "/value")))

;POST 	/session/:sessionId/forward
(defun forward(session)
  (send-command
    session
    :post
    ((session-data-base session) "/forward")))

;POST 	/session/:sessionId/keys
(defun send-keys(session keys)
  (send-command
    session
    :post
    ((session-data-base session) "/keys")))

(defun send-string(session string)
  (send-keys session keys))

;GET 	/session/:sessionId/location
(defun location(session)
  (send-command
    session
    :get
    ((session-data-base session) "/location")))

;POST 	/session/:sessionId/moveto
(defun move-to(session)
  (send-command
    session
    :post
    ((session-data-base session) "/moveto")))

;GET 	/session/:sessionId/orientation
(defun orientation(session)
  (send-command
    session
    :get
    ((session-data-base session) "/orientation")))

;GET 	/session/:sessionId/screenshot
(defun take-screenshot(session)
  (send-command
    session
    :get
    ((session-data-base session) "/screenshot")))

;GET 	/session/:sessionId/source
(defun get-source(session)
  (send-command
    session
    :get
    ((session-data-base session) "/source")))

;POST 	/session/:sessionId/timeouts
(defun set-timeouts(session)
  (send-command
    session
    :post
    ((session-data-base session) "/timeouts")))

;GET 	/session/:sessionId/title
(defun get-title(session)
  (send-command
    session
    :get
    ((session-data-base session) "/title")))

;POST 	/session/:sessionId/touch/click
(defun touch-click(session)
  (send-command
    session
    :post
    ((session-data-base session) "/touch/click")))

;POST 	/session/:sessionId/touch/doubleclick
(defun touch-doubleclick(session)
  (send-command
    session
    :post
    ((session-data-base session) "/touch/doubleclick")))

;POST 	/session/:sessionId/touch/down
(defun touch-down(session)
  (send-command
    session
    :post
    ((session-data-base session) "/touch/down")))

;POST 	/session/:sessionId/touch/flick
(defun touch-flick(session)
  (send-command
    session
    :post
    ((session-data-base session) "/touch/flick")))

;POST 	/session/:sessionId/touch/longclick
(defun touch-longclick(session)
  (send-command
    session
    :post
    ((session-data-base session) "/touch/longclick")))

;POST 	/session/:sessionId/touch/move
(defun touch-move(session)
  (send-command
    session
    :post
    ((session-data-base session) "/touch/move")))

;POST 	/session/:sessionId/touch/scroll
(defun touch-scroll(session)
  (send-command
    session
    :post
    ((session-data-base session) "/touch/scroll")))

;POST 	/session/:sessionId/touch/up
(defun touch-up(session)
  (send-command
    session
    :post
    ((session-data-base session) "/touch/up")))

;DELETE 	/session/:sessionId/window
(defun close-window(session)
  (send-command
    session
    :delete
    ((session-data-base session) "/window")))

;POST 	/session/:sessionId/window
(defun switch-to-window(session)
  (send-command
    session
    :post
    ((session-data-base session) "/window")))

;POST 	/session/:sessionId/window/maximize
(defun maximize-window(session)
  (send-command
    session
    :post
    ((session-data-base session) "/window/maximize")))

;POST 	/session/:sessionId/window/size
(defun get-window-size(session)
  (send-command
    session
    :post
    ((session-data-base session) "/window/size")))

;GET 	/session/:sessionId/window/size
(defun set-window-size(session)
  (send-command
    session
    :get
    ((session-data-base session) "/window/size")))

;POST 	/session/:sessionId/window/:windowHandle/size
(defun get-window-size-with-window-handle(session window-handle)
  (send-command
    session
    :post
    ((session-data-base session) "/window/" window-handle "size")))

;GET 	/session/:sessionId/window/:windowHandle/size
(defun set-window-size-with-window-handle(session window-handle)
  (send-command
    session
    :get
    ((session-data-base session) "/window/" window-handle "/size")))

;POST 	/session/:sessionId/window/:windowHandle/position
(defun set-window-position-with-window-handle(session window-handle)
  (send-command
    session
    :post
    ((session-data-base session) "/window/" window-handle "/position")))

;GET 	/session/:sessionId/window/:windowHandle/position
(defun get-window-position-with-window-handle(session window-handle)
  (send-command
    session
    :get
    ((session-data-base session) "/window/" window-handle "/position")))

;POST 	/session/:sessionId/window/:windowHandle/maximize
(defun window-maximize-with-window-handle(session window-handle)
  (send-command
    session
    :post
    ((session-data-base session) "/window/" window-handle "/maximize")))

;GET 	/session/:sessionId/window_handle
(defun get-window-handle(session)
  (send-command
    session
    :get
    ((session-data-base session) "/window_handle")))

;GET 	/session/:sessionId/window_handles
(defun get-window-handles(session)
  (send-command
    session
    :get
    ((session-data-base session) "/window_handles")))

