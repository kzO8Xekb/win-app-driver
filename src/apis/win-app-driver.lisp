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

(defparameter +http-header-accept+       "application/json, image/png")
(defparameter +http-header-content-type+ "application/json; charset=UTF-8")
(defparameter +empty-data+               "{\"\": \"\"}")

(defstruct session-data
  (capabilities nil)
  (host          "")
  (port           0)
  (id            "")
  (base          ""))

;;; HTTP/HTTPS Utilities
(lol:defmacro! protect-for-timeout (&rest body)
 `(let
    (,g!dexador-response-hash-table
     ,g!dexador-response-quri-uri
     ,g!server-response-message
     ,g!server-response-status-code
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
          (setf
            (values
              ,g!server-response-message
              ,g!server-response-status-code
              ,g!dexador-response-hash-table
              ,g!dexador-response-quri-uri)
            (progn ,@body))
          (go ,g!tag-finish)))
      ,g!tag-finish)
    (values ,g!server-response-message ,g!server-response-status-code ,g!dexador-response-hash-table ,g!dexador-response-quri-uri)))

;(declaim (inline post-json))
(defun post-json (uri content)
  "uriのエンドポイントにjsonをPOSTします。"
  (dex:post uri
            :content content
            :headers `(("accept"       . ,+http-header-accept+)
                       ("accept-type"  . ,+http-header-accept+)
                       ("content-type" . ,+http-header-content-type+))))

;(declaim (inline post-json2))
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

(lol:defmacro! invoke-win-app-driver-api%0 (func endpoint &key (accessor #'get-value) (content nil))
 `(let
    ((,g!response (jonathan:parse
                    (protect-for-timeout
                      ,(if (null content)
                         `(funcall ,func ,endpoint)
                         `(funcall ,func ,endpoint ,content))))))
    (values (funcall ,accessor ,g!response) ,g!response)))

(lol:defmacro! invoke-win-app-driver-api (func endpoint &key (content nil))
 `(multiple-value-bind
    (,g!server-response-message ,g!server-response-status-code ,g!dexador-response-hash-table ,g!dexador-response-quri-uri)
    (protect-for-timeout
      ,(if (null content)
         `(funcall ,func ,endpoint)
         `(funcall ,func ,endpoint ,content)))
    (values
      ,g!server-response-message
      ,g!server-response-status-code
      ,g!dexador-response-hash-table
      ,g!dexador-response-quri-uri)))

(defun ad-get (endpoint) ; adはapplication driverの省略。
  (invoke-win-app-driver-api #'dexador:get endpoint))

(defun ad-delete (endpoint)
  (invoke-win-app-driver-api #'dexador:delete endpoint))

(defun ad-post (endpoint content)
  (invoke-win-app-driver-api
    #'post-json
    endpoint
    :content content))

(defun ipv4-address-p (host-identifier)
  (multiple-value-bind
    (start end)
    (cl-ppcre:scan
      "^(([1-9]?[0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\.){3}([1-9]?[0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])$"
      host-identifier)
    (values start end)))

(defun rfc4291-address-p (host-identifier)
  "引数で指定した文字列がRFC4291に適合するIPv6アドレスの文字列表現であるか照合します。"
  (multiple-value-bind
    (start end)
    (cl-ppcre:scan
      "^((([0-9A-Fa-f]{1,4}:){7}([0-9A-Fa-f]{1,4}|:))|(([0-9A-Fa-f]{1,4}:){6}(:[0-9A-Fa-f]{1,4}|((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3})|:))|(([0-9A-Fa-f]{1,4}:){5}(((:[0-9A-Fa-f]{1,4}){1,2})|:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3})|:))|(([0-9A-Fa-f]{1,4}:){4}(((:[0-9A-Fa-f]{1,4}){1,3})|((:[0-9A-Fa-f]{1,4})?:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9A-Fa-f]{1,4}:){3}(((:[0-9A-Fa-f]{1,4}){1,4})|((:[0-9A-Fa-f]{1,4}){0,2}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9A-Fa-f]{1,4}:){2}(((:[0-9A-Fa-f]{1,4}){1,5})|((:[0-9A-Fa-f]{1,4}){0,3}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9A-Fa-f]{1,4}:){1}(((:[0-9A-Fa-f]{1,4}){1,6})|((:[0-9A-Fa-f]{1,4}){0,4}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(:(((:[0-9A-Fa-f]{1,4}){1,7})|((:[0-9A-Fa-f]{1,4}){0,5}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:)))(%.+)?$"
      host-identifier)
    (values start end)))

(defun ipv6-address-p (host-identifier)
  "引数で指定した文字列が\"[\"と\"]\"で囲まれたIPv6アドレスの文字列表現であるか照合します。"
  (multiple-value-bind
    (start end)
    (cl-ppcre:scan
      "^\\[((([0-9A-Fa-f]{1,4}:){7}([0-9A-Fa-f]{1,4}|:))|(([0-9A-Fa-f]{1,4}:){6}(:[0-9A-Fa-f]{1,4}|((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3})|:))|(([0-9A-Fa-f]{1,4}:){5}(((:[0-9A-Fa-f]{1,4}){1,2})|:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3})|:))|(([0-9A-Fa-f]{1,4}:){4}(((:[0-9A-Fa-f]{1,4}){1,3})|((:[0-9A-Fa-f]{1,4})?:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9A-Fa-f]{1,4}:){3}(((:[0-9A-Fa-f]{1,4}){1,4})|((:[0-9A-Fa-f]{1,4}){0,2}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9A-Fa-f]{1,4}:){2}(((:[0-9A-Fa-f]{1,4}){1,5})|((:[0-9A-Fa-f]{1,4}){0,3}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9A-Fa-f]{1,4}:){1}(((:[0-9A-Fa-f]{1,4}){1,6})|((:[0-9A-Fa-f]{1,4}){0,4}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(:(((:[0-9A-Fa-f]{1,4}){1,7})|((:[0-9A-Fa-f]{1,4}){0,5}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:)))(%.+)?\\]$"
      host-identifier)
    (values start end)))

(defun hostname-p (host-identifier)
  "引数で指定した文字列がRFC952及びRFC1123に適合する文字列か照合します。"
  (flet
    ((label-length-p (itr)
                     (and
                       (<= 1 (length host-identifier) 255)
                       (<= 1 (length itr) 63)))
     (label-character-p (itr)
                        (multiple-value-bind
                          (start end)
                          (ppcre:scan "^(?!.*-$)[0-9A-Za-z\\-]+$" itr)
                          (values start end))))
    (let
      ((start 0)
       (end   0))
      (loop :for itr :in (ppcre:split "\\." host-identifier)
            :do (progn
                  (cond
                    ((and (label-length-p itr) (label-character-p itr))
                     (incf end (length itr)))
                    (t (setf start nil
                             end   nil)
                       (return)))))
      (values start end))))

(declaim (ftype (function (string) (values (or string null) (member t) &optional)) correct-hostname-string-p))
(defun correct-hostname-string-p (host-identifier)
"correct-hostname-string-p
_/_/_/ 概要 _/_/_/
引数で指定した文字列が，ホスト名に適合するか照合します。
文字列がマッチした場合は，第1返り値を引数で指定した文字列，第2返り値をtとした多値で返します。
マッチしなければ，第1及び第2返り値ともにnilとした多値で返します。

_/_/_/ 引数 _/_/_/
host-identifier, string: 照合したい文字列を指定します。

_/_/_/ 返り値 _/_/_/
string or nil, 照合成功した場合は引数で指定した文字列。失敗時はnil。
t or nil,      関数の成功時はt，失敗時はnil。"
  (let
    (start end)
    (cond
      ((setf (values start end) (ipv4-address-p host-identifier))
       (values host-identifier t))
      ((setf (values start end) (ipv6-address-p host-identifier)) ; 正確にはRFC4291ではない。
       (values host-identifier t))
      ((setf (values start end) (hostname-p host-identifier))
       (values host-identifier t))
      (t (values nil t)))))

(defun correct-port-number-p (number)
  (declare (type (integer number)))
  (and
    (eq
      'integer
      (aif (type-of number)
           (if (listp it)
             (car it)
             it)))
    (<= 1 number 65535)))

(defun get-win-app-driver-host-uri (session)
  (make-condition-if
    (complement #'correct-hostname-string-p)
    ((session-data-host session))
    error
    'condition-incorrect-hostname-string
    'get-win-app-driver-host-uri)
  (make-condition-if
    (complement #'correct-port-number-p)
    ((session-data-port session))
    error
    'condition-incorrect-port-number
    'get-win-app-driver-host-uri)
  (concatenate
    'string
    (session-data-host session)
    ":"
    (write-to-string
      (session-data-port session))))

(defun generate-endpoint-uri (session &rest directories)
  (apply
    #'concatenate
    `(string
      "http://"
      ,(get-win-app-driver-host-uri session)
      ,@directories)))

(defmacro send-command (session command-type endpoint &optional (content nil))
  (cond
    ((eq command-type :get)
     `(progn
        (ad-get
          (generate-endpoint-uri ,session ,@endpoint))))
    ((eq command-type :post)
     `(progn
        (ad-post
          (generate-endpoint-uri ,session ,@endpoint)
          ,content)))
    ((eq command-type :delete)
     `(progn
        (ad-delete
          (generate-endpoint-uri ,session ,@endpoint))))
    (t `(progn
          (error
            (make-condition
              'condition-unknown-http-command-error
              :caller 'send-command))))))

;; Command Summary
;; see https://github.com/microsoft/WinAppDriver/blob/master/Docs/SupportedAPIs.md
; The following is a list of APIs supported by WinAppDriver:
; HTTP 	Path

; Status
; GET /status
(defun status ()
  (ad-get
    (generate-endpoint-uri "/status")
    :accessor #'get-value))

;(defun status ()
;  (send-command :get ("/status")))

; New Session
(defun make-desired-capabilities (&key
                                   (app nil)
                                   (app-arguments nil)
                                   (app-top-level-window nil)
                                   (app-working-dir nil)
                                   (device-name "WindowsPC")
                                   (platform-name "Windows")
                                   (platform-version nil))
  "make-desired-capabilities
_/_/_/ 概要 _/_/_/
WinAppDriverとのsessionを確立するために必要なdesired capabilitiesを作成します。

_/_/_/ 引数 _/_/_/
app,                  [key]app:               Application identifier or executable full path. ex. Microsoft.MicrosoftEdge_8wekyb3d8bbwe!MicrosoftEdge
app-arguments,        [key]appArguments:      Application launch arguments. ex. https://github.com/Microsoft/WinAppDriver
app-top-level-window, [key]appTopLevelWindow: Existing application top level window to attach to. ex. 0xB822E2
app-working-dir,      [key]appWorkingDir:     Application working directory (Classic apps only). ex. C:\Temp
device-name,          [key]deviceName:        Application working device type name. ex. WindowsPC
platform-name,        [key]platformName:      Target platform name. ex. Windows
platform-version,     [key]platformVersion:   Target platform version. ex. 1.0"
(concatenate
  'string
  (jonathan:to-json
    `(:|desiredCapabilities|
       (,@(aif app
               `(:|app| ,it))
         ,@(aif app-arguments
                `(:|appArguments| ,it))
         ,@(aif app-top-level-window
                `(:|appTopLevelWindow| ,it))
         ,@(aif app-working-dir
                `(:|appWorkingDir| ,it))
         ,@(aif device-name
                `(:|deviceName| ,it))
         ,@(aif platform-name
                `(:|platformName| ,it))
         ,@(aif platform-version
                `(:|platformVersion| ,it)))))))

; New Session
; POST /session
(defun new-session (session)
  (send-command
    session
    :post
    ("/session")
    (session-data-capabilities session)))

;GET 	/sessions
;(defapi get-sessions :get ("/sessions"))

;DELETE 	/session/:sessionId
(defun delete-session (session)
  (send-command session :delete ((session-data-base session))))

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
    ((session nil))
    (let
      (impl self)
      (setq
        impl (lol:dlambda
               (:new-session (&key
                               (app                  nil)
                               (app-arguments        nil)
                               (app-top-level-window nil)
                               (app-working-dir      nil)
                               (device-name          "WindowsPC")
                               (host                 "localhost")
                               (platform-name        "Windows")
                               (platform-version     nil)
                               (port                 4723))
                             (setf session (make-session-data
                                             :host host
                                             :port port
                                             :capabilities (make-desired-capabilities
                                                             :app                  app
                                                             :app-arguments        app-arguments
                                                             :app-top-level-window app-top-level-window
                                                             :app-working-dir      app-working-dir
                                                             :device-name          device-name
                                                             :platform-name        platform-name
                                                             :platform-version     platform-version)))
                             (multiple-value-bind
                               (json status-code hash quri)
                               (new-session session)
                               (progn
                                 (setf (session-data-id session) (getf (jonathan:parse json) :|sessionId|))
                                 (setf (session-data-base session)
                                       (concatenate
                                         'string
                                         "/session/"
                                         (session-data-id session)
                                         "/"))
                                 (values json status-code hash quri session))))
               (:delete-session ()
                                (cond
                                  (session
                                    (multiple-value-bind
                                      (json status-code hash quri)
                                      (delete-session session)
                                      (progn
                                        (setf session nil)
                                        (values json status-code hash quri))))
                                  (t (values nil nil nil nil)))))

        self (lambda (&rest args)
               (apply impl args))))))

