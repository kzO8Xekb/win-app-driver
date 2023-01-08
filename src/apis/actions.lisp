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

; All functions in this file are unimplemented.
; Even if it works, it has not been tested.

;; Command Summary
;; see https://github.com/microsoft/WinAppDriver/blob/master/Docs/SupportedAPIs.md
; The following is a list of APIs supported by WinAppDriver:
; HTTP 	Path

; HTTP Command:  POST
; Path:         /session/:sessionId/buttondown
; This function is not yet implemented.
(defun button-down (session)
  (send-command
    session
    :post
    ((session-data-base session) "/buttondown")))

; HTTP Command:  POST
; Path:         /session/:sessionId/buttonup
; This function is not yet implemented.
(defun button-up (session)
  (send-command
    session
    :post
    ((session-data-base session) "/buttonup")))

; HTTP Command:  POST
; Path:         /session/:sessionId/click
; This function is not yet implemented.
(defun click (session)
  (send-command
    session
    :post
    ((session-data-base session) "/click")))

; HTTP Command:  POST
; Path:         /session/:sessionId/doubleclick
; This function is not yet implemented.
(defun doubleclick (session)
  (send-command
    session
    :post
    ((session-data-base session) "/doubleclick")))

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
(defun move-to (session)
  (send-command
    session
    :post
    ((session-data-base session) "/moveto")))

; HTTP Command:  POST
; Path:         /session/:sessionId/touch/click
; This function is not yet implemented.
(defun touch-click (session)
  (send-command
    session
    :post
    ((session-data-base session) "/touch/click")))

; HTTP Command:  POST
; Path:         /session/:sessionId/touch/doubleclick
; This function is not yet implemented.
(defun touch-doubleclick (session)
  (send-command
    session
    :post
    ((session-data-base session) "/touch/doubleclick")))

; HTTP Command:  POST
; Path:         /session/:sessionId/touch/down
; This function is not yet implemented.
(defun touch-down (session)
  (send-command
    session
    :post
    ((session-data-base session) "/touch/down")))

; HTTP Command:  POST
; Path:         /session/:sessionId/touch/flick
; This function is not yet implemented.
(defun touch-flick (session)
  (send-command
    session
    :post
    ((session-data-base session) "/touch/flick")))

; HTTP Command:  POST
; Path:         /session/:sessionId/touch/longclick
; This function is not yet implemented.
(defun touch-longclick (session)
  (send-command
    session
    :post
    ((session-data-base session) "/touch/longclick")))

; HTTP Command:  POST
; Path:         /session/:sessionId/touch/move
; This function is not yet implemented.
(defun touch-move (session)
  (send-command
    session
    :post
    ((session-data-base session) "/touch/move")))

; HTTP Command:  POST
; Path:         /session/:sessionId/touch/scroll
; This function is not yet implemented.
(defun touch-scroll (session)
  (send-command
    session
    :post
    ((session-data-base session) "/touch/scroll")))

; HTTP Command:  POST
; Path:         /session/:sessionId/touch/up
; This function is not yet implemented.
(defun touch-up (session)
  (send-command
    session
    :post
    ((session-data-base session) "/touch/up")))

