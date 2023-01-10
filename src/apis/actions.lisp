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
(defun button-down (session button)
  (send-command
    session
    :post
    ((session-data-base session) "/buttondown")
    (generate-mouse-button-content button)))

; HTTP Command:  POST
; Path:         /session/:sessionId/buttonup
(defun button-up (session button)
  (send-command
    session
    :post
    ((session-data-base session) "/buttonup")
    (generate-mouse-button-content button)))

; HTTP Command:  POST
; Path:         /session/:sessionId/click
(defun click (session button)
  (send-command
    session
    :post
    ((session-data-base session) "/click")
    (generate-mouse-button-content button)))

; HTTP Command:  POST
; Path:         /session/:sessionId/doubleclick
(defun double-click (session)
  (send-command
    session
    :post
    ((session-data-base session) "/doubleclick")))

; HTTP Command:  POST
; Path:         /session/:sessionId/moveto

; I could not come up with an implementation method that would avoid the use of SBCL-specific functions.
(sb-c:defknown move-to (SESSION-DATA T T &optional T) *) ; In this case, an error should be made.

(sb-c:deftransform move-to ((session element-id xoffset &optional yoffset) (SESSION-DATA STRING INTEGER INTEGER) *) 
  `(move-to-impl1 session element-id xoffset yoffset))

(sb-c:deftransform move-to ((session xoffset yoffset &optional _) (SESSION-DATA INTEGER INTEGER NIL) *) 
  `(move-to-impl2 session xoffset yoffset))

(defun move-to (session arg1 arg2 &optional (arg3 nil))
  (cond
    ((and
       (session-data-p session)
       (integerp       arg1)
       (integerp       arg2)
       (null           arg3))
     (move-to-impl2 session arg1 arg2))
    ((and
       (session-data-p session)
       (stringp        arg1)
       (integerp       arg2)
       (integerp       arg3))
     (move-to-impl1 session arg1 arg2 arg3))
    (t 
      (error
        (make-condition
          'win-app-driver:condition-incorrect-arguments
          :caller      'move-to
          :caller-args (session arg1 arg2 arg3))))))

(defun move-to-impl1 (session element-id xoffset &optional yoffset)
  (flet
    ((generate-content (element-id xoffset yoffset)
                         (jonathan:to-json
                           `(:|element| ,element-id :|xoffset| ,xoffset :|yoffset| ,yoffset))))
    (send-command
      session
      :post
      ((session-data-base session) "/moveto")
      (generate-content element-id xoffset yoffset))))

(defun move-to-impl2 (session xoffset yoffset &optional _)
  (declare (ignorable _))
  (flet
    ((generate-content (xoffset yoffset)
                         (jonathan:to-json
                           `(:|xoffset| ,xoffset :|yoffset| ,yoffset))))
    (send-command
      session
      :post
      ((session-data-base session) "/moveto")
      (generate-content xoffset yoffset))))

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
(sb-c:defknown touch-flick (SESSION-DATA T T &optional T T) *) ; In this case, an error should be made.

(sb-c:deftransform touch-flick ((session xoffset yoffset &optional _1 _2) (SESSION-DATA INTEGER INTEGER NIL NIL) *) 
  `(touch-flick-impl1 session xoffset yoffset))

(sb-c:deftransform touch-flick ((session element-id xoffset &optional yoffset speed) (SESSION-DATA STRING INTEGER INTEGER INTEGER) *) 
  `(touch-flick-impl2 session element-id xoffset yoffset))

(defun touch-flick (session arg1 arg2 &optional (arg3 nil) (arg4 nil))
  (cond
    ((and
       (session-data-p session)
       (integerp       arg1)
       (integerp       arg2)
       (null           arg3)
       (null           arg4))
     (touch-flick-impl1 session arg1 arg2))
    ((and
       (session-data-p session)
       (stringp        arg1)
       (integerp       arg2)
       (integerp       arg3)
       (integerp       arg4))
     (touch-flick-impl2 session arg1 arg2 arg3 arg4))
    (t 
      (error
        (make-condition
          'win-app-driver:condition-incorrect-arguments
          :caller      'touch-flick
          :caller-args (session arg1 arg2 arg3 arg4))))))

(defun touch-flick-impl1 (session xspeed yspeed &optional (_1 nil) (_2 nil))
  (declare (ignorable _1 _2))
  (flet
    ((generate-content (xspeed yspeed)
                       (jonathan:to-json
                         `(:|xspeed| ,xspeed :|yspeed| ,yspeed))))
    (send-command
      session
      :post
      ((session-data-base session) "/touch/flick")
      (generate-content xspeed yspeed))))

(defun touch-flick-impl2 (session element-id xoffset &optional yoffset speed)
  (flet
    ((generate-content (xoffset yoffset)
                       (jonathan:to-json
                         `(:|element| ,element-id :|xoffset| ,xoffset :|yoffset| ,yoffset :|speed| ,speed))))
    (send-command
      session
      :post
      ((session-data-base session) "/touch/flick")
      (generate-content xoffset yoffset))))

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
(sb-c:defknown touch-scroll (session-data t t &optional t) *) ; In this case, an error should be made.

(sb-c:deftransform touch-scroll ((session xoffset yoffset &optional _) (session-data integer integer nil) *) 
  `(touch-scroll-impl1 session xoffset yoffset))

(sb-c:deftransform touch-scroll ((session element-id xoffset &optional yoffset) (session-data string integer integer) *) 
  `(touch-scroll-impl2 session element-id xoffset yoffset))

(defun touch-scroll (session arg1 arg2 &optional (arg3 nil))
  (cond
    ((and
       (session-data-p session)
       (integerp       arg1)
       (integerp       arg2)
       (null           arg3))
     (touch-scroll-impl1 session arg1 arg2))
    ((and
       (session-data-p session)
       (stringp        arg1)
       (integerp       arg2)
       (integerp       arg3))
     (touch-scroll-impl2 session arg1 arg2 arg3))
    (t 
      (error
        (make-condition
          'win-app-driver:condition-incorrect-arguments
          :caller      'touch-scroll
          :caller-args (session arg1 arg2 arg3))))))

(defun touch-scroll-impl1 (session xoffset yoffset &optional (_ nil))
  (declare (ignorable _))
  (flet
    ((generate-content (xoffset yoffset)
                       (jonathan:to-json
                         `(:|xoffset| ,xoffset :|yoffset| ,yoffset))))
    (send-command
      session
      :post
      ((session-data-base session) "/touch/scroll")
      (generate-content xoffset yoffset))))

(defun touch-scroll-impl2 (session element-id xoffset &optional yoffset)
  (flet
    ((generate-content (element-id xoffset yoffset)
                       (jonathan:to-json
                         `(:|element| ,element-id :|xoffset| ,xoffset :|yoffset| ,yoffset))))
    (send-command
      session
      :post
      ((session-data-base session) "/touch/scroll")
      (generate-content element-id xoffset yoffset))))

; HTTP Command:  POST
; Path:         /session/:sessionId/touch/up
(defun touch-up (session x y)
  (send-command
    session
    :post
    ((session-data-base session) "/touch/up")
    (generate-content-of-position x y)))

