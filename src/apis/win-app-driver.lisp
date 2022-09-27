;;; MIT License
;;; 
;;; Copyright (c) 2022 kzO8Xekb
;;; 
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be included in all
;;; copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.

(in-package :win-app-driver)

;;; HTTP/HTTPS Utilities
(lol:defmacro! protect-for-timeout (&rest body)
 `(let
    (,g!result
      (,g!sleep-time -1))
    (tagbody
      ,g!tag-start
      (handler-bind
        ((error
           (lambda (condition)
             (when (= 12002 (winhttp::win-error-code condition)) ; timeout
               (progn
                 (incf ,g!sleep-time)
                 (sleep ,g!sleep-time)
                 (go ,g!tag-start)))))) ; やり直し
        (progn
          (setf ,g!result (progn ,@body))
          (go ,g!tag-finish)))
      ,g!tag-finish)
    ,g!result))

(defparameter empty-data "{\"\": \"\"}")

(declaim (inline post-json))
(defun post-json (uri content)
  "uriのエンドポイントにjsonをPOSTします。"
  (dex:post uri
            :content content
            :headers `(("accept-type" . "application/json")
                       ("content-type" . "application/json; charset=UTF-8"))))

(declaim (inline post-json2))
(defun post-json2 (uri content)
  "uriのエンドポイントにjsonをPOSTします。"
  (post-json uri (jonathan:to-json content)))

;(defun post-json-after-parse (uri content)
;  "uriのエンドポイントにjsonをPOSTします。
;  その後，結果をjonathanでパースしたものを返します。"
;  (jonathan:parse
;    (post-json uri content)))
;
;(defun post-json2-after-parse (uri content)
;  "uriのエンドポイントにjsonをPOSTします。
;  その後，結果をjonathanでパースしたものを返します。"
;  (jonathan:parse
;    (post-json2 uri content)))

(lol:defmacro! invoke-win-app-driver-api (func endpoint accessor &rest content)
 `(let
    ((,g!response (jonathan:parse
                    (protect-for-timeout
                      (funcall
                        ,func
                        (concatenate 'string ,endpoint)
                        ,@content)))))
    (values (funcall ,accessor ,g!response) ,g!response)))

(defmacro ad-get (endpoint accessor)
  `(invoke-win-app-driver-api #'dexador:get ,endpoint ,accessor))

(defmacro ad-delete (endpoint accessor)
  `(invoke-win-app-driver-api #'dexador:delete ,endpoint ,accessor))

(defmacro ad-post (endpoint accessor &rest content)
  `(invoke-win-app-driver-api #'post-json2 ,endpoint ,accessor ,@content))

(declaim (inline get-win-app-driver-host-uri))
(defun get-win-app-driver-host-uri (session)
  (concatenate
    'string
    (funcall session :pandoric-get 'host)
    ":"
    (write-to-string
      (funcall session :pandoric-get 'port))))

(defmacro generate-endpoint-uri (session &rest directories)
 `(concatenate
    'string
    (get-win-app-driver-host-uri ,session)
    ,@directories))

(defmacro send-command (command-type session endpoint &rest content &key (accessor #'get-value))
  (cond
    ((eq command-type :get)
     `(ad-get (generate-endpoint-uri ,session ,@endpoint) ,accessor))
    ((eq command-type :post)
     `(ad-post2 (generate-endpoint-uri ,session ,@endpoint) ,accessor ,@content))
    ((eq command-type :delete)
     `(ad-delete (generate-endpoint-uri ,session ,@endpoint) ,accessor))
    (t (error "unknown http command error."))))

;; Command Summary
;; see https://github.com/microsoft/WinAppDriver/blob/master/Docs/SupportedAPIs.md
; The following is a list of APIs supported by WinAppDriver:
; HTTP 	Path

; Status
; GET /status
(defun status ()
  (ad-get
    (generate-endpoint-uri "/status")
    #'get-value))

;(defun status ()
;  (send-command :get ("/status")))

; New Session
(defun make-desired-capabilities
  (&key
    (app nil)
    (app-arguments nil)
    (app-top-level-window nil)
    (app-working-dir nil)
    (platform-name "Windows")
    (platform-version nil))
  "make-desired-capabilities
   _/_/_/ 概要 _/_/_/
   WinAppDriverとのsessionを確立するために必要なdesired capabilitiesを作成します。
   
   _/_/_/ 引数 _/_/_/
   app,                  [key]app: Application identifier or executable full path. ex. Microsoft.MicrosoftEdge_8wekyb3d8bbwe!MicrosoftEdge
   app-arguments,        [key]appArguments: Application launch arguments. ex. https://github.com/Microsoft/WinAppDriver
   app-top-level-window, [key]appTopLevelWindow: Existing application top level window to attach to. ex. 0xB822E2
   app-working-dir,      [key]appWorkingDir: Application working directory (Classic apps only). ex. C:\Temp
   platform-name,        [key]platformName: Target platform name. ex. Windows
   platform-version,     [key]platformVersion: Target platform version. ex. 1.0"
  (concatenate
    'string
    "{\"desiredCapabilities\":{"
    (aif app
         "\"app\":\"" it "\""
         "")
    (aif app-arguments
         ",\"appArguments\":\"" it "\""
         "")
    (aif app-top-level-window
         ",\"appTopLevelWindow\":\"" it "\""
         "")
    (aif app-working-dir
         ",\"appWorkingDir\":\"" it "\""
         "")
    ",\"platformName\":\"" platform-name "\""
    (aif platform-version
         ",\"platformVersion\":\"" it "\""
         "")
    "}}"))

; POST /session
;(defapi new-session :post nil)

;GET 	/sessions
;(defapi get-sessions :get ("/sessions"))

;DELETE 	/session/:sessionId
;(defapi delete-session :delete (session-base session-id))

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

(defun create-session ()
  (lol:pandoriclet
    ((capabilities nil)
     (host "localhost")
     (port 4723)
     (session-id nil)
     (session-base nil))
    (lol:dlambda
      (:status () (status)))))

