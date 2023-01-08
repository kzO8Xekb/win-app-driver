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

(defparameter *find-element-value-regex*  "^\{\"ELEMENT\":\"[0-9\.\-]+\"\}$")
(defparameter *find-elements-value-regex* "^\\\[(\\\{\"ELEMENT\":\"[0-9\.\-]+\"\\\})?(,\\\{\"ELEMENT\":\"[0-9\.\-]+\"\\\})*\\\]$")
(defparameter *session-id-regex*          "[0-9A-F]{1,8}\-[0-9A-F]{1,4}\-[0-9A-F]{1,4}\-[0-9A-F]{1,4}\-[0-9A-F]{1,12}")
(defparameter *window-handle-regex*       "0x[0-9A-F]{1,8}")
(defparameter *base64-regex*              "^[A-Za-z0-9\/\+]+=*$")

(lol:defmacro! test-api (test-code
                          &key
                          (session-id       t)
                          (status           0)
                          (app              `(is
                                               (getf (win-app-driver::get-value $json) :|app|)
                                               nil))
                          (platform-name    `(is
                                               (getf (win-app-driver::get-value $json) :|platformName|)
                                               nil))
                          (http-status-code 200)
                          (server           "Microsoft-HTTPAPI/2.0")
                          (content-type     "application/json")
                          (content-length   0)
                          (protcol          "http")
                          (host             *win-app-driver-host*)
                          (port             *win-app-driver-port*)
                          (path             "")
                          (value            `(is
                                               (win-app-driver::get-value $json)
                                               nil)))
               `(multiple-value-bind
                  ($json $code $hash $quri)
                  ,test-code
                  (progn
                    (when ,session-id
                      (like
                        (getf (jonathan:parse $json) :|sessionId|)
                        (concatenate 'string "^" win-app-driver/tests::*session-id-regex* "$")))
                    (is
                      (getf (jonathan:parse $json) :|status|)
                      ,status)
                    ,app
                    ,platform-name
                    (is
                      $code
                      ,http-status-code)
                    (is-values
                      (gethash "server" $hash)
                      '(,server t))
                    (is-values
                      (gethash "content-type" $hash)
                      '(,content-type t))
                    (is-values
                      (gethash "content-length" $hash)
                      `(,,content-length t))
                    (is
                      (quri:uri-scheme $quri)
                      ,protcol)
                    (is
                      (quri:uri-host $quri)
                      ,host)
                    (is
                      (quri:uri-port $quri)
                      ,port)
                    (is
                      (quri:uri-path $quri)
                      ,path)
                    ,value)))
;(multiple-value-bind
;  ($0 $1 $2 $3)
;  (funcall notepad-session
;           :new-session
;           :host            *win-app-driver-host*
;           :port            *win-app-driver-port*
;           :app             "C:/Windows/System32/notepad.exe")
;  (progn
;    (like
;      (getf (jonathan:parse $0) :|sessionId|)
;      ;"FBF1B803-4579-44EA-B84B-AA49DC1C5344")
;      "^[0-9A-F]{1,8}\-[0-9A-F]{1,4}\-[0-9A-F]{1,4}\-[0-9A-F]{1,4}\-[0-9A-F]{1,12}$")
;    (is
;      (getf (jonathan:parse $0) :|status|)
;      0)
;    (is
;      (getf (getf (jonathan:parse $0) :|value|) :|app|)
;      "C:/Windows/System32/notepad.exe")
;    (is
;      (getf (getf (jonathan:parse $0) :|value|) :|platformName|)
;      "Windows")
;    (is
;      $1
;      200)
;    (is-values
;      (gethash "server" $2)
;      '("Microsoft-HTTPAPI/2.0" t))
;    (is-values
;      (gethash "content-type" $2)
;      '("application/json" t))
;    (is-values
;      (gethash "content-length" $2)
;      '("138" t))
;    (is
;      (quri:uri-scheme $3)
;      "http")
;    (is
;      (quri:uri-host $3)
;      *win-app-driver-host*)
;    (is
;      (quri:uri-port $3)
;      *win-app-driver-port*)
;    (is
;      (quri:uri-path $3)
;      "/session")
;    (setf
;      json $0
;      code $1
;      hash $2
;      quri $3)))

