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

(defparameter *session-id-regex* "[0-9A-F]{1,8}\-[0-9A-F]{1,4}\-[0-9A-F]{1,4}\-[0-9A-F]{1,4}\-[0-9A-F]{1,12}")

(lol:defmacro! test-api (test-code
                          &key
                          (session-id       t)
                          (status           0)
                          (app              "")
                          (platform-name    "Windows")
                          (http-status-code 200)
                          (server           "Microsoft-HTTPAPI/2.0")
                          (content-type     "application/json")
                          (content-length   0)
                          (protcol          "http")
                          (host             *win-app-driver-host*)
                          (port             *win-app-driver-port*)
                          (path             ""))
               `(multiple-value-bind
                  (,g!json ,g!code ,g!hash ,g!quri)
                  ,test-code
                  (progn
                    (when ,session-id
                      (like
                        (getf (jonathan:parse ,g!json) :|sessionId|)
                        (concatenate 'string "^" win-app-driver/tests::*session-id-regex* "$")))
                    (is
                      (getf (jonathan:parse ,g!json) :|status|)
                      ,status)
                    (is
                      (getf (getf (jonathan:parse ,g!json) :|value|) :|app|)
                      ,app)
                    (is
                      (getf (getf (jonathan:parse ,g!json) :|value|) :|platformName|)
                      ,platform-name)
                    (is
                      ,g!code
                      ,http-status-code)
                    (is-values
                      (gethash "server" ,g!hash)
                      '(,server t))
                    (is-values
                      (gethash "content-type" ,g!hash)
                      '(,content-type t))
                    (is-values
                      (gethash "content-length" ,g!hash)
                      '(,content-length t))
                    (is
                      (quri:uri-scheme ,g!quri)
                      ,protcol)
                    (is
                      (quri:uri-host ,g!quri)
                      ,host)
                    (is
                      (quri:uri-port ,g!quri)
                      ,port)
                    (is
                      (quri:uri-path ,g!quri)
                      ,path))))
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
             :app            "C:/Windows/System32/notepad.exe"
             :path           "/session"
             :content-length "138")

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
               "^/session/" *session-id-regex* "/$"))
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
                   'win-app-driver::session))
               "/")))

  (subtest "Testing find-element."
           ;(test-api
           ;  (funcall
           ;    notepad-session
           ;    :find-element
           ;    :name "Text editor")
           ;  :app            nil
           ;  :platform-name  nil
           ;  :content-length "96"
           ;  :path           (concatenate
           ;                    'string
           ;                    base
           ;                    "/element"))
           (test-api
             (funcall
               notepad-session
               :find-element
               :class-name "RichEditD2DPT")
             :app            nil
             :platform-name  nil
             :content-length "96"
             :path           (concatenate
                               'string
                               base
                               "element")))
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
               :app            nil
               :platform-name  nil
               :content-length "63"
               :path           (concatenate
                                 'string
                                 base
                                 "element/"
                                 element-id
                                 "/click")))

    (subtest "Testing send-string."
             (test-api
               (funcall
                 notepad-session
                 :send-string
                 "Hello, World!"
                 :enter)
               :app            nil
               :platform-name  nil
               :content-length "63"
               :path           (concatenate
                                 'string
                                 base
                                 "keys")))

    (subtest "Testing close-window."
             (test-api
               (funcall
                 notepad-session
                 :close-window)
               :app            nil
               :platform-name  nil
               :content-length "63"
               :path           (concatenate
                                 'string
                                 base
                                 "window")))

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
                 :app            nil
                 :platform-name  nil
                 :content-length "63"
                 :path           (concatenate
                                   'string
                                   base
                                   "element/"
                                   element-id
                                   "/click"))))
    )

  (subtest "Testing delete-session."
           (test-api
             (funcall notepad-session :delete-session)
             :session-id     nil
             :app            nil
             :platform-name  nil
             :content-length "12"
             :path           base))
  )

