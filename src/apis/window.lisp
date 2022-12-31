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

; Close Window
; HTTP Command: DELETE
; Path:         /session/:sessionId/window
(defun close-window (session)
  (send-command
    session
    :delete
    ((session-data-base session) "/window")))

; POST 	/session/:sessionId/window
; This function is not yet implemented. It is temporarily stored in "api.lisp".
; Uncomment it after implementation and testing.
;(defun switch-to-window (session)
;  (send-command
;    session
;    :post
;    ((session-data-base session) "/window")))

; Maximize Window
; HTTP Command:  POST
; Path:         /session/:sessionId/window/maximize
(defun maximize-window (session)
  (send-command
    session
    :post
    ((session-data-base session) "/window/maximize")))

(declaim (inline generate-content-of-window-size))
(defun generate-content-of-window-size (width height)
  (jonathan:to-json `(:|height| ,height :|width| ,width)))

; Set Window Size
; HTTP Command: POST
; Path:         /session/:sessionId/window/size
(defun set-window-size (session width height)
  (send-command
    session
    :post
    ((session-data-base session) "/window/size")
    (generate-content-of-window-size width height)))

; Get Window Size
; HTTP Command: GET
; Path:         /session/:sessionId/window/size
(defun get-window-size (session)
  (send-command
    session
    :get
    ((session-data-base session) "/window/size")))

; Set WindowSize with WindowHandle
; HTTP Command: POST
; Path:         /session/:sessionId/window/:windowHandle/size
(defun set-window-size-with-window-handle (session window-handle width height)
  (send-command
    session
    :post
    ((session-data-base session) "/window/" window-handle "/size")
    (generate-content-of-window-size width height)))

; Get WindowSize with WindowHandle
; HTTP Command: GET
; Path:         /session/:sessionId/window/:windowHandle/size
(defun get-window-size-with-window-handle (session window-handle)
  (send-command
    session
    :get
    ((session-data-base session) "/window/" window-handle "/size")))

(declaim (inline generate-content-of-window-position))
(defun generate-content-of-window-position (x y)
  (jonathan:to-json `(:|x| ,x :|y| ,y)))

; Set WindowPosition with WindowHandle
; HTTP Command: POST
; Path:         /session/:sessionId/window/:windowHandle/position
(defun set-window-position-with-window-handle (session window-handle x y)
  (send-command
    session
    :post
    ((session-data-base session) "/window/" window-handle "/position")
    (generate-content-of-window-position x y)))

; Get WindowPosition with WindowHandle
; HTTP Command: GET
; Path:         /session/:sessionId/window/:windowHandle/position
(defun get-window-position-with-window-handle (session window-handle)
  (send-command
    session
    :get
    ((session-data-base session) "/window/" window-handle "/position")))


; Window Maximize with WindowHandle
; HTTP Command:  POST
; Path:         /session/:sessionId/window/:windowHandle/maximize
(defun window-maximize-with-window-handle (session window-handle)
  (send-command
    session
    :post
    ((session-data-base session) "/window/" window-handle "/maximize")))

