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
; modified from Microsoft sample codes
; https://github.com/microsoft/WinAppDriver/blob/master/Docs/AuthoringTestScripts.md

;// Launch Notepad
;DesiredCapabilities appCapabilities = new DesiredCapabilities();
;appCapabilities.SetCapability("app", @"C:\Windows\System32\notepad.exe");
;appCapabilities.SetCapability("appArguments", @"MyTestFile.txt");
;appCapabilities.SetCapability("appWorkingDir", @"C:\MyTestFolder\");
;NotepadSession = new WindowsDriver<WindowsElement>(new Uri("http://127.0.0.1:4723"), appCapabilities);
(let
  ((session1 (win-app-driver::create-session))
   (session2 (win-app-driver::create-session))
   base)
  (subtest "Testing new-session."
           (test-api
             (funcall
               session1
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
                          session1
                          :pandoric-get
                          'win-app-driver::session)))

           ; initialized session-data check.
           (like
             (win-app-driver::session-data-id
               (funcall
                 session1
                 :pandoric-get
                 'win-app-driver::session))
             (concatenate
               'string
               "^" *session-id-regex* "$"))
           (is
             (win-app-driver::session-data-capabilities
               (funcall
                 session1
                 :pandoric-get
                 'win-app-driver::session))
             "{\"desiredCapabilities\":{\"app\":\"C:/Windows/System32/notepad.exe\",\"deviceName\":\"WindowsPC\",\"platformName\":\"Windows\"}}")
           (is
             (win-app-driver::session-data-host
               (funcall
                 session1
                 :pandoric-get
                 'win-app-driver::session))
             *win-app-driver-host*)
           (is
             (win-app-driver::session-data-port
               (funcall
                 session1
                 :pandoric-get
                 'win-app-driver::session))
             *win-app-driver-port*)
           (like
             (win-app-driver::session-data-base
               (funcall
                 session1
                 :pandoric-get
                 'win-app-driver::session))
             (concatenate
               'string
               "^/session/" *session-id-regex* "$"))
           (is
             (win-app-driver::session-data-base
               (funcall
                 session1
                 :pandoric-get
                 'win-app-driver::session))
             (concatenate
               'string
               "/session/"
               (win-app-driver::session-data-id
                 (funcall
                   session1
                   :pandoric-get
                   'win-app-driver::session)))))

  (funcall session2
           :new-session
           :host        *win-app-driver-host*
           :port        *win-app-driver-port*
           :app         "Microsoft.WindowsCalculator_8wekyb3d8bbwe!App")

  (subtest "Testing get-sessions."
           (test-api
             (funcall session1 :get-sessions)
             :session-id     nil
             :content-length  (write-to-string
                                (length $json))
             :path            "/sessions"
             :app             (ok
                                (win-app-driver::get-value $json))
             :platform-name   (ok
                                (win-app-driver::get-value $json))
             :value           (ok
                                (win-app-driver::get-value $json))))

  (subtest "Testing delete-session."
           (test-api
             (funcall session1 :delete-session)
             :session-id     nil
             :content-length "12"
             :path           base))

  (funcall session2 :delete-session)
  )

