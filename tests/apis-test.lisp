;;;; MIT License
;;;; 
;;;; Copyright (c) 2016 Microsoft
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

;; NOTE: To run this test file, execute `(asdf:test-system :win-app-driver)' in your Lisp.

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

; modified from Microsoft sample codes
; https://github.com/microsoft/WinAppDriver/blob/master/Docs/AuthoringTestScripts.md

;// Launch Notepad
;DesiredCapabilities appCapabilities = new DesiredCapabilities();
;appCapabilities.SetCapability("app", @"C:\Windows\System32\notepad.exe");
;appCapabilities.SetCapability("appArguments", @"MyTestFile.txt");
;appCapabilities.SetCapability("appWorkingDir", @"C:\MyTestFolder\");
;NotepadSession = new WindowsDriver<WindowsElement>(new Uri("http://127.0.0.1:4723"), appCapabilities);
(let
  ((notepad-session (win-app-driver::create-session))
   base)
  (subtest "Testing new-session."
           (test-api
             (funcall
               notepad-session
               :new-session
               :host           *win-app-driver-host*
               :port           *win-app-driver-port*
               :app            "C:/Windows/System32/notepad.exe")
             :app            (is
                               (getf
                                 (win-app-driver::get-value $json)
                                 :|app|)
                               "C:/Windows/System32/notepad.exe")
             :platform-name  (is
                               (getf
                                 (win-app-driver::get-value $json)
                                 :|platformName|)
                               "Windows")
             :path           "/session"
             :content-length "138"
             :value          (is
                               (win-app-driver::get-value $json)
                               `(:|platformName| "Windows" :|app| "C:/Windows/System32/notepad.exe")))

           (setf base (win-app-driver::session-data-base
                        (funcall
                          notepad-session
                          :pandoric-get
                          'win-app-driver::session)))

           ; initialized session-data check.
           (like
             (win-app-driver::session-data-id
               (funcall
                 notepad-session
                 :pandoric-get
                 'win-app-driver::session))
             (concatenate
               'string
               "^" *session-id-regex* "$"))
           (is
             (win-app-driver::session-data-capabilities
               (funcall
                 notepad-session
                 :pandoric-get
                 'win-app-driver::session))
             "{\"desiredCapabilities\":{\"app\":\"C:/Windows/System32/notepad.exe\",\"deviceName\":\"WindowsPC\",\"platformName\":\"Windows\"}}")
           (is
             (win-app-driver::session-data-host
               (funcall
                 notepad-session
                 :pandoric-get
                 'win-app-driver::session))
             *win-app-driver-host*)
           (is
             (win-app-driver::session-data-port
               (funcall
                 notepad-session
                 :pandoric-get
                 'win-app-driver::session))
             *win-app-driver-port*)
           (like
             (win-app-driver::session-data-base
               (funcall
                 notepad-session
                 :pandoric-get
                 'win-app-driver::session))
             (concatenate
               'string
               "^/session/" *session-id-regex* "$"))
           (is
             (win-app-driver::session-data-base
               (funcall
                 notepad-session
                 :pandoric-get
                 'win-app-driver::session))
             (concatenate
               'string
               "/session/"
               (win-app-driver::session-data-id
                 (funcall
                   notepad-session
                   :pandoric-get
                   'win-app-driver::session)))))

  ; Find Element
  (subtest "Testing find-element."
           (test-api
             (funcall
               notepad-session
               :find-element
               :class-name "RichEditD2DPT")
             :content-length (write-to-string
                               (+ 72
                                  (length
                                    (jonathan:to-json
                                      (win-app-driver::get-value $json)))))
             :path           (concatenate
                               'string
                               base
                               "/element")
             :value         (like
                              (jonathan:to-json
                                (win-app-driver::get-value $json))
                              *find-element-value-regex*)))

  (subtest "Testing find-elements."
           (test-api
             (funcall
               notepad-session
               :find-elements
               :class-name
               "Microsoft.UI.Xaml.Controls.MenuBarItem")
             :content-length (write-to-string
                               (+ 72
                                  (length
                                    (jonathan:to-json
                                      (win-app-driver::get-value $json)))))
             :path           (concatenate
                               'string
                               base
                               "/elements")
             :app           (ok
                              (win-app-driver::get-value $json))
             :platform-name (ok
                              (win-app-driver::get-value $json))
             :value         (like
                              (jonathan:to-json
                                (win-app-driver::get-value $json))
                              *find-elements-value-regex*)))

  (let
    ((element-id (getf
                   (win-app-driver::get-value
                     (funcall
                       notepad-session
                       :find-element
                       :automation-id
                       "MenuBar"))
                   :|ELEMENT|)))
    (subtest "Testing find-element-from-element."
             (test-api
               (funcall
                 notepad-session
                 :find-element-from-element
                 element-id
                 :class-name
                 "Microsoft.UI.Xaml.Controls.MenuBarItem")
               :content-length (write-to-string
                                 (+ 72
                                    (length
                                      (jonathan:to-json
                                        (win-app-driver::get-value $json)))))
               :path           (concatenate
                                 'string
                                 base
                                 "/element/"
                                 element-id
                                 "/element")
               :value         (like
                                (jonathan:to-json
                                  (win-app-driver::get-value $json))
                                *find-element-value-regex*)))

    (subtest "Testing find-elements-from-element."
             (test-api
               (funcall
                 notepad-session
                 :find-elements-from-element
                 element-id
                 :class-name
                 "Microsoft.UI.Xaml.Controls.MenuBarItem")
               :content-length (write-to-string
                                 (+ 72
                                    (length
                                      (jonathan:to-json
                                        (win-app-driver::get-value $json)))))
               :path           (concatenate
                                 'string
                                 base
                                 "/element/"
                                 element-id
                                 "/elements")
               :app           (ok
                                (win-app-driver::get-value $json))
               :platform-name (ok
                                (win-app-driver::get-value $json))
               :value         (like
                                (jonathan:to-json
                                  (win-app-driver::get-value $json))
                                *find-elements-value-regex*))))

  ; Element Manipulation
  (let*
    ((response (funcall
                 notepad-session
                 :find-element
                 :class-name "RichEditD2DPT"))
     (element-id (get-element-id response)))
    (subtest "Testing element-click."
             (test-api
               (funcall
                 notepad-session
                 :element-click
                 element-id)
               :content-length "63"
               :path           (concatenate
                                 'string
                                 base
                                 "/element/"
                                 element-id
                                 "/click")))

    (subtest "Testing send-string."
             (test-api
               (funcall
                 notepad-session
                 :send-string
                 "Hello, World!"
                 :enter)
               :content-length "63"
               :path           (concatenate
                                 'string
                                 base
                                 "/keys")))

    (subtest "Testing element-clear."
             (test-api
               (funcall
                 notepad-session
                 :element-clear
                 element-id)
               :content-length "63"
               :path           (concatenate
                                 'string
                                 base
                                 "/element/"
                                 element-id
                                 "/clear")))

    (subtest "Testing element-send-string."
             (test-api
               (funcall
                 notepad-session
                 :element-send-string
                 element-id
                 :control
                 "a"
                 :control
                 :delete
                 "Hello, World!"
                 :enter)
               :content-length "63"
               :path           (concatenate
                                 'string
                                 base
                                 "/element/"
                                 element-id
                                 "/value")))

    (subtest "Testing get-element-name."
             (test-api
               (funcall
                 notepad-session
                 :get-element-name
                 element-id)
               :content-length "94"
               :path           (concatenate
                                 'string
                                 base
                                 "/element/"
                                 element-id
                                 "/name")
               :app            (is
                                 (win-app-driver::get-value $json)
                                 "ControlType.Document")
               :platform-name  (is
                                 (win-app-driver::get-value $json)
                                 "ControlType.Document")
               :value          (is
                                 (win-app-driver::get-value $json)
                                 "ControlType.Document")))

    (subtest "Testing active-element."
             (test-api
               (funcall
                 notepad-session
                 :active-element)
               :content-length (write-to-string
                                 (+ 72
                                    (length
                                      (jonathan:to-json
                                        (win-app-driver::get-value $json)))))
               :path           (concatenate
                                 'string
                                 base
                                 "/element/active")
               :value          (like
                                 (jonathan:to-json
                                   (win-app-driver::get-value $json))
                                 *find-element-value-regex*)))

    (subtest "Testing is-element-enabled."
             (test-api
               (funcall
                 notepad-session
                 :is-element-enabled
                 element-id)
               :content-length "76"
               :path           (concatenate
                                 'string
                                 base
                                 "/element/"
                                 element-id
                                 "/enabled")
               :app            (ok
                                 (win-app-driver::get-value $json))
               :platform-name  (ok
                                 (win-app-driver::get-value $json))
               :value          (ok
                                 (win-app-driver::get-value $json))))

    (subtest "Testing is-element-displayed."
             (test-api
               (funcall
                 notepad-session
                 :is-element-displayed
                 element-id)
               :content-length "76"
               :path           (concatenate
                                 'string
                                 base
                                 "/element/"
                                 element-id
                                 "/displayed")
               :app            (ok
                                 (win-app-driver::get-value $json))
               :platform-name  (ok
                                 (win-app-driver::get-value $json))
               :value          (ok
                                 (win-app-driver::get-value $json))))

    (subtest "Testing is-element-selected."
             (test-api
               (funcall
                 notepad-session
                 :is-element-selected
                 element-id)
               :content-length "77"
               :path           (concatenate
                                 'string
                                 base
                                 "/element/"
                                 element-id
                                 "/selected")))

    (subtest "Testing element-equals."
             (test-api
               (funcall
                 notepad-session
                 :element-equals
                 element-id
                 element-id)
               :content-length "76"
               :path           (concatenate
                                 'string
                                 base
                                 "/element/"
                                 element-id
                                 "/equals/"
                                 element-id)
               :app            (ok
                                 (win-app-driver::get-value $json))
               :platform-name  (ok
                                 (win-app-driver::get-value $json))
               :value          (ok
                                 (win-app-driver::get-value $json)))

             (let
               ((menu-bar-element-id (get-element-id
                                       (funcall
                                         notepad-session
                                         :find-element
                                         :automation-id "MenuBar"))))
               (test-api
                 (funcall
                   notepad-session
                   :element-equals
                   element-id
                   menu-bar-element-id)
                 :content-length "77"
                 :path           (concatenate
                                   'string
                                   base
                                   "/element/"
                                   element-id
                                   "/equals/"
                                   menu-bar-element-id))))

    (subtest "Testing get-element-attribute."
             (test-api
               (funcall
                 notepad-session
                 :get-element-attribute
                 element-id
                 "FrameworkId")
               :content-length (write-to-string
                                 (+ 72
                                    (length
                                      (jonathan:to-json
                                        (win-app-driver::get-value $json)))))
               :path           (concatenate
                                 'string
                                 base
                                 "/element/"
                                 element-id
                                 "/attribute/FrameworkId")
               :app            (is
                                 (win-app-driver::get-value $json)
                                 "Win32")
               :platform-name  (is
                                 (win-app-driver::get-value $json)
                                 "Win32")
               :value          (is
                                 (win-app-driver::get-value $json)
                                 "Win32")))

    (subtest "Testing get-element-size."
             (test-api
               (funcall
                 notepad-session
                 :get-element-size
                 element-id)
               :content-length "99"
               :path           (concatenate
                                 'string
                                 base
                                 "/element/"
                                 element-id
                                 "/size")
               :value          (is
                                 (win-app-driver::get-value $json)
                                 `(:|width| 1147 :|height| 571))))

    (subtest "Testing get-element-locaion."
             (test-api
               (funcall
                 notepad-session
                 :get-element-location
                 element-id)
               :content-length "86"
               :path           (concatenate
                                 'string
                                 base
                                 "/element/"
                                 element-id
                                 "/location")
               :value          (is
                                 (win-app-driver::get-value $json)
                                 `(:|y| 90 :|x| 0))))

    (subtest "Testing get-element-location-in-view."
             (test-api
               (funcall
                 notepad-session
                 :get-element-location-in-view
                 element-id)
               :content-length "86"
               :path           (concatenate
                                 'string
                                 base
                                 "/element/"
                                 element-id
                                 "/location_in_view")
               :value          (is
                                 (win-app-driver::get-value $json)
                                 `(:|y| 90 :|x| 0))))

    (subtest "Testing generate-content-of-window-size."
             (is
               (win-app-driver::generate-content-of-timeouts
                 :implicit 12345)
               "{\"implicit\":12345}"))

    (subtest "Testing set-timeouts."
             (test-api
               (funcall
                 notepad-session
                 :set-timeouts
                 :implicit 2500)
               :content-length "63"
               :path           (concatenate
                                 'string
                                 base
                                 "/timeouts")))

    (subtest "Testing get-window-handle."
             (test-api
               (funcall
                 notepad-session
                 :get-window-handle)
               :app            (like
                                 (win-app-driver::get-value $json)
                                 (concatenate
                                   'string
                                   "^" *window-handle-regex* "$"))
               :content-length "84"
               :path           (concatenate
                                 'string
                                 base
                                 "/window_handle")
               :platform-name  (like
                                 (win-app-driver::get-value $json)
                                 (concatenate
                                   'string
                                   "^" *window-handle-regex* "$"))
               :value          (like
                                 (win-app-driver::get-value $json)
                                 (concatenate
                                   'string
                                   "^" *window-handle-regex* "$"))))

    (subtest "Testing get-window-handles."
             (test-api
               (funcall
                 notepad-session
                 :get-window-handles)
               :app            (like
                                 (first (win-app-driver::get-value $json))
                                 (concatenate
                                   'string
                                   "^" *window-handle-regex* "$"))
               :content-length "86"
               :path           (concatenate
                                 'string
                                 base
                                 "/window_handles")
               :platform-name  (like
                                 (first (win-app-driver::get-value $json))
                                 (concatenate
                                   'string
                                   "^" *window-handle-regex* "$"))
               :value          (like
                                 (jonathan:to-json
                                   (win-app-driver::get-value $json))
                                 (concatenate
                                   'string
                                   "^\\["
                                   "(\\\""
                                   *window-handle-regex*
                                   "\\\")?"
                                   "(\,\\\""
                                   *window-handle-regex*
                                   "\\\")*"
                                   "\\]$"))))

    (subtest "Testing get-window-size."
             (test-api
               (funcall
                 notepad-session
                 :get-window-size)
               :content-length "99"
               :path           (concatenate
                                 'string
                                 base
                                 "/window/size")
               :value          (like
                                 (jonathan:to-json
                                   (win-app-driver::get-value $json))
                                 "\{\"width\":[0-9]+,\"height\":[0-9]+\}")))

    ;; Testing window size.
    (subtest "Testing generate-content-of-window-size."
             (is
               (win-app-driver::generate-content-of-window-size 123 456)
               "{\"height\":456,\"width\":123}"))

    (let*
      ((response (funcall
                   notepad-session
                   :get-window-size))
       (before-width    (getf (win-app-driver::get-value response) :|width|))
       (before-height   (getf (win-app-driver::get-value response) :|height|))
       (after-width 0)
       (after-height 0)
       (handle (win-app-driver::get-value
                 (funcall
                   notepad-session
                   :get-window-handle))))
      (subtest "Testing set-window-size."
               (test-api
                 (funcall
                   notepad-session
                   :set-window-size
                   387
                   456)
                 :content-length "63"
                 :path           (concatenate
                                   'string
                                   base
                                   "/window/size"))
               (setf
                 response     (funcall notepad-session :get-window-size)
                 after-width  (getf (win-app-driver::get-value response) :|width|)
                 after-height (getf (win-app-driver::get-value response) :|height|))
               (is
                 (getf (win-app-driver::get-value response) :|width|)
                 387)
               (is
                 (getf (win-app-driver::get-value response) :|height|)
                 456))

      (funcall
        notepad-session
        :set-window-size
        before-width
        before-height)

      (subtest "Testing set-window-size-with-window-handle."
               (test-api
                 (funcall
                   notepad-session
                   :set-window-size-with-window-handle
                   handle
                   387
                   456)
                 :content-length "63"
                 :path           (concatenate
                                   'string
                                   base
                                   "/window/"
                                   handle
                                   "/size"))
               (setf
                 response     (funcall notepad-session :get-window-size)
                 after-width  (getf (win-app-driver::get-value response) :|width|)
                 after-height (getf (win-app-driver::get-value response) :|height|))
               (is
                 (getf (win-app-driver::get-value response) :|width|)
                 387)
               (is
                 (getf (win-app-driver::get-value response) :|height|)
                 456))

      (subtest "Testing maximize-window."
               (test-api
                 (funcall
                   notepad-session
                   :maximize-window)
                 :content-length "63"
                 :path           (concatenate
                                   'string
                                   base
                                   "/window/maximize"))
               (setf
                 response     (funcall notepad-session :get-window-size)
                 after-width  (getf (win-app-driver::get-value response) :|width|)
                 after-height (getf (win-app-driver::get-value response) :|height|))
               (is
                 (getf (win-app-driver::get-value response) :|width|)
                 1922)
               (is
                 (getf (win-app-driver::get-value response) :|height|)
                 1030))

      (funcall
        notepad-session
        :set-window-size
        before-width
        before-height)

      (subtest "Testing window-maximize-with-window-handle."
               (test-api
                 (funcall
                   notepad-session
                   :window-maximize-with-window-handle
                   handle)
                 :content-length "63"
                 :path           (concatenate
                                   'string
                                   base
                                   "/window/"
                                   handle
                                   "/maximize"))
               (setf
                 response     (funcall notepad-session :get-window-size)
                 after-width  (getf (win-app-driver::get-value response) :|width|)
                 after-height (getf (win-app-driver::get-value response) :|height|))
               (is
                 (getf (win-app-driver::get-value response) :|width|)
                 1922)
               (is
                 (getf (win-app-driver::get-value response) :|height|)
                 1030))

      (funcall
        notepad-session
        :set-window-size
        before-width
        before-height)

      )

    (subtest "Testing generate-content-of-window-position."
             (is
               (win-app-driver::generate-content-of-window-position 123 456)
               "{\"x\":123,\"y\":456}"))

    (subtest "Testing get-window-position-with-window-handle."
             (let
               ((handle (win-app-driver::get-value
                          (funcall
                            notepad-session
                            :get-window-handle))))
               (test-api
                 (funcall
                   notepad-session
                   :get-window-position-with-window-handle
                   handle)
                 :content-length "88"
                 :path           (concatenate
                                   'string
                                   base
                                   "/window/"
                                   handle
                                   "/position")
                 :value          (like
                                   (jonathan:to-json
                                     (win-app-driver::get-value $json))
                                   "\{\"y\":[0-9]+,\"x\":[0-9]+\}"))))

    (let*
      ((handle (win-app-driver::get-value
                 (funcall
                   notepad-session
                   :get-window-handle)))
       (response (funcall
                   notepad-session
                   :get-window-position-with-window-handle
                   handle))
       (x        (getf (win-app-driver::get-value response) :|x|))
       (y        (getf (win-app-driver::get-value response) :|y|)))
      (subtest "testing set-window-position-with-window-handle."
               (test-api
                 (funcall
                   notepad-session
                   :set-window-position-with-window-handle
                   handle
                   433
                   88)
                 :content-length "63"
                 :path           (concatenate
                                   'string
                                   base
                                   "/window/"
                                   handle
                                   "/position"))
               (setf response (funcall
                                notepad-session
                                :get-window-position-with-window-handle
                                handle))
               (is
                 (getf (win-app-driver::get-value response) :|x|)
                 433)
               (is
                 (getf (win-app-driver::get-value response) :|y|)
                 88))

      (funcall
        notepad-session
        :set-window-position-with-window-handle
        handle
        x
        y)

      )

    ; Test screenshots.
    ; The return value of the screenshot is Base64-encoded text data in png format.
    ; The image can be viewed by decoding it using a package such as cl-base64.
    (subtest "Testing take-screenshot."
             (test-api
               (funcall
                 notepad-session
                 :take-screenshot)
               :content-length (write-to-string
                                 (+ 74 (length (win-app-driver::get-value $json))))
               :path           (concatenate
                                 'string
                                 base
                                 "/screenshot")
               :app            (like
                                 (win-app-driver::get-value $json)
                                 *base64-regex*)
               :platform-name  (like
                                 (win-app-driver::get-value $json)
                                 *base64-regex*)
               :value          (like
                                 (win-app-driver::get-value $json)
                                 *base64-regex*)))

    ; ToDo take-element-screenshot.

    (subtest "Testing orientation."
             (test-api
               (funcall
                 notepad-session
                 :orientation)
               :content-length "83"
               :path           (concatenate
                                 'string
                                 base
                                 "/orientation")
               ; The return value depends on the environment.
               ; Essentially, various environment values should be set and tested.
               :app            (is
                                 (win-app-driver::get-value $json)
                                 "LANDSCAPE")
               :platform-name  (is
                                 (win-app-driver::get-value $json)
                                 "LANDSCAPE")
               :value          (is
                                 (win-app-driver::get-value $json)
                                 "LANDSCAPE")))

    (subtest "Testing get-source."
               (test-api
                 (funcall
                   notepad-session
                   :get-source)
                 :content-length (write-to-string
                                   (+ 1252 (length (win-app-driver::get-value $json))))
                 :path           (concatenate
                                   'string
                                   base
                                   "/source")
                 :app            (ok
                                   (win-app-driver::get-value $json))
                 :platform-name  (ok
                                   (win-app-driver::get-value $json))
                 :value          (ok
                                   (win-app-driver::get-value $json))))

    (subtest "Testing get-title."
               (test-api
                 (funcall
                   notepad-session
                   :get-title)
                 :content-length (write-to-string
                                   (+ 92 (length (win-app-driver::get-value $json))))
                 :path           (concatenate
                                   'string
                                   base
                                   "/title")
                 :app            (ok
                                   (win-app-driver::get-value $json))
                 :platform-name  (ok
                                   (win-app-driver::get-value $json))
                 :value          (ok
                                   (win-app-driver::get-value $json))))

    (subtest "Testing close-window."
             (test-api
               (funcall
                 notepad-session
                 :close-window)
               :content-length "63"
               :path           (concatenate
                                 'string
                                 base
                                 "/window")))

    (let
      ((element-id (get-element-id
                     (funcall
                       notepad-session
                       :find-element
                       :automation-id "SecondaryButton"))))
      (subtest "Testing element-click."
               (test-api
                 (funcall
                   notepad-session
                   :element-click
                   element-id)
                 :content-length "63"
                 :path           (concatenate
                                   'string
                                   base
                                   "/element/"
                                   element-id
                                   "/click"))))
    )

  (subtest "Testing delete-session."
           (test-api
             (funcall notepad-session :delete-session)
             :session-id     nil
             :content-length "12"
             :path           base))
  )

