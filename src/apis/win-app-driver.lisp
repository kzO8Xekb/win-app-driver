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

(defmacro generate-endpoint-uri (&rest directories)
  `(concatenate
     'string
     ,(get-win-app-driver-uri)
     ,@directories))

(defmacro send-command (command-type endpoint &rest content &key (accessor #'get-value))
  `(cond
     ((eq ,command-type :get)
      (ad-get (generate-endpoint-uri ,@endpoint) ,accessor))
     ((eq ,command-type :post)
      (ad-post2 (generate-endpoint-uri ,@endpoint) ,accessor ,@content))
     ((eq ,command-type :delete)
      (ad-delete (generate-endpoint-uri ,@endpoint) ,accessor))
     (t (error "unknown http command error."))))

(let
  ((win-app-driver-host "localhost")
   (win-app-driver-port 4723)
   (session-base nil))

  (declare (inline get-win-app-driver-uri))
  (defun get-win-app-driver-uri ()
    (concatenate 'string win-app-driver-host ":" (write-to-string win-app-driver-port)))

    ;; Command Summary
  ;; see https://github.com/microsoft/WinAppDriver/blob/master/Docs/SupportedAPIs.md
  ; The following is a list of APIs supported by WinAppDriver:
  ; HTTP 	Path
  
  ; Get Status
  ; GET /status
  ;(defun get-status ()
  ;  (ad-get
  ;    #'dexador:get
  ;    (generate-endpoint-uri "/status")))
  (defun get-status ()
    (send-command :get ("/status")))

  ; New Session
  (defun generate-desired-capabilities (options)
    (declare (ignorable options))
    (if options
      (jonathan:to-json `(:|desiredCapabilities| (:|ms:edgeOptions| ,options)))
      "{\"desiredCapabilities\":{}}"))                                                        ; capabilities だと element-send-keys で入力できない不具合が発生したので修正。

  ; POST /session
  (defapi new-session :post nil)

  ;GET 	/sessions
  (defapi get-sessions :get ("/sessions"))

  ;DELETE 	/session/:sessionId
  (defapi delete-session :delete (session-base session-id))

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
  )
