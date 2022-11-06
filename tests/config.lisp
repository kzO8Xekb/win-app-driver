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

(in-package :win-app-driver/tests)

(defparameter *win-app-driver-host* "127.0.0.1")
(defparameter *win-app-driver-port* 12345)

;(lol:defmacro! with-win-app-driver-server (&body body)
; `(flet
;    ((,g!win-app-driver-server-terminate (,g!process-info)
;                                         (when (uiop:process-alive-p ,g!process-info)
;                                           (uiop:terminate-process ,g!process-info))))
;    (let
;      ((,g!win-app-driver-server-process-info nil))
;      (unwind-protect
;        (progn
;          (setf ,g!win-app-driver-server-process-info
;                (uiop:launch-program
;                  (concatenate
;                    'string
;                    "WinAppDriver.exe "
;                    ,*win-app-driver-host*
;                    " "
;                    (write-to-string ,*win-app-driver-port*))))
;          ,@body)
;        (,g!win-app-driver-server-terminate ,g!win-app-driver-server-process-info)))))

