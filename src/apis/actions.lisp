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

; HTTP Command:  POST
; Path:         /session/:sessionId/buttondown
; This function is not yet implemented.
(defun button-down (session x y)
  (send-command
    session
    :post
    ((session-data-base session) "/buttondown")
    (generate-content-of-position x y)))

; HTTP Command:  POST
; Path:         /session/:sessionId/buttonup
; This function is not yet implemented.
(defun button-up (session x y)
  (send-command
    session
    :post
    ((session-data-base session) "/buttonup")
    (generate-content-of-position x y)))

; HTTP Command:  POST
; Path:         /session/:sessionId/click
; This function is not yet implemented.
(defun click (session x y)
  (send-command
    session
    :post
    ((session-data-base session) "/click")
    (generate-content-of-position x y)))

; HTTP Command:  POST
; Path:         /session/:sessionId/doubleclick
; This function is not yet implemented.
(defun doubleclick (session x y)
  (send-command
    session
    :post
    ((session-data-base session) "/doubleclick")
    (generate-content-of-position x y)))

; HTTP Command:  GET
; Path:         /session/:sessionId/location
; This function is not yet implemented.
(defun location (session)
  (send-command
    session
    :get
    ((session-data-base session) "/location")))

; HTTP Command:  POST
; Path:         /session/:sessionId/moveto
; This function is not yet implemented.
(defun move-to (session x y)
  (send-command
    session
    :post
    ((session-data-base session) "/moveto")
    (generate-content-of-position x y)))

; HTTP Command:  POST
; Path:         /session/:sessionId/touch/click
(defun touch-click (session element-id)
  (send-command
    session
    :post
    ((session-data-base session) "/touch/click")
    (generate-content-of-touch-element element-id)))

; HTTP Command:  POST
; Path:         /session/:sessionId/touch/doubleclick
(defun touch-double-click (session element-id)
  (send-command
    session
    :post
    ((session-data-base session) "/touch/doubleclick")
    (generate-content-of-touch-element element-id)))

; HTTP Command:  POST
; Path:         /session/:sessionId/touch/down
(defun touch-down (session x y)
  (send-command
    session
    :post
    ((session-data-base session) "/touch/down")
    (generate-content-of-position x y)))

; HTTP Command:  POST
; Path:         /session/:sessionId/touch/flick
(defun touch-flick (session xspeed yspeed)
  (send-command
    session
    :post
    ((session-data-base session) "/touch/flick")
    (jonathan:to-json
      `(:|xspeed| ,xspeed :|yspeed| ,yspeed))))

; HTTP Command:  POST
; Path:         /session/:sessionId/touch/longclick
(defun touch-long-click (session element-id)
  (send-command
    session
    :post
    ((session-data-base session) "/touch/longclick")
    (generate-content-of-touch-element element-id)))

; HTTP Command:  POST
; Path:         /session/:sessionId/touch/move
(defun touch-move (session x y)
  (send-command
    session
    :post
    ((session-data-base session) "/touch/move")
    (generate-content-of-position x y)))

; HTTP Command:  POST
; Path:         /session/:sessionId/touch/scroll
(defun touch-scroll (session element-id xoffset yoffset)
  (send-command
    session
    :post
    ((session-data-base session) "/touch/scroll")
    (jonathan:to-json
      `(:|element| ,element-id :|xoffset| ,xoffset :|yoffset| ,yoffset))))

; HTTP Command:  POST
; Path:         /session/:sessionId/touch/up
(defun touch-up (session x y)
  (send-command
    session
    :post
    ((session-data-base session) "/touch/up")
    (generate-content-of-position x y)))

