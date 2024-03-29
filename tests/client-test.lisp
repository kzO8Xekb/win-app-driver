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

;; NOTE: To run this test file, execute `(asdf:test-system :win-app-driver)' in your Lisp.

(subtest "Testing ipv6-address-p"
         (is-values
           (win-app-driver::ipv6-address-p "::1")
           '(nil nil))
         (is-values
           (win-app-driver::ipv6-address-p "[::1]")
           '(0 5))
         )

(subtest "Testing hostname-p"
         (is-values
           (win-app-driver::hostname-p "localhost")
           '(0 9)))

(subtest "Testing correct-hostname-string-p"
         (is-values
           (win-app-driver::correct-hostname-string-p "localhost")
           '("localhost" t))
         (is-values
           (win-app-driver::correct-hostname-string-p "0.0.0.0")
           '("0.0.0.0" t))
         (is-values
           (win-app-driver::correct-hostname-string-p "127.0.0.1")
           '("127.0.0.1" t))
         (is-values
           (win-app-driver::correct-hostname-string-p "192.168.123.45")
           '("192.168.123.45" t))
         (is-values
           (win-app-driver::correct-hostname-string-p "255.255.255.255")
           '("255.255.255.255" t))
         (is-values
           (win-app-driver::correct-hostname-string-p "256.255.255.255")
           '("256.255.255.255" t))
         (is-values
           (win-app-driver::correct-hostname-string-p "255.256.255.255")
           '("255.256.255.255" t))
         (is-values
           (win-app-driver::correct-hostname-string-p "255.255.256.255")
           '("255.255.256.255" t))
         (is-values
           (win-app-driver::correct-hostname-string-p "255.255.255.256")
           '("255.255.255.256" t))
         (is-values
           (win-app-driver::correct-hostname-string-p "128.128.128.128")
           '("128.128.128.128" t))
         (is-values
           (win-app-driver::correct-hostname-string-p "[::1]")
           '("[::1]" t)))

(subtest "Testing get-win-app-driver-host-uri"
         (let
           ((session (win-app-driver::make-session-data)))
           ; check host name
           (setf (session-data-host session) "localhost")
           (setf (win-app-driver::session-data-port session) 12345)
           (is
             (win-app-driver::get-win-app-driver-host-uri session)
             "localhost:12345")

           (setf (win-app-driver::session-data-host session) "foo.bar.baz")
           (is
             (win-app-driver::get-win-app-driver-host-uri session)
             "foo.bar.baz:12345")

           ; check IPv4 address
           (setf (win-app-driver::session-data-host session) "127.0.0.1")
           (setf (win-app-driver::session-data-port session) 54321)
           (is
             (win-app-driver::get-win-app-driver-host-uri session)
             "127.0.0.1:54321")

           (setf (win-app-driver::session-data-host session) "192.168.123.45")
           (is
             (win-app-driver::get-win-app-driver-host-uri session)
             "192.168.123.45:54321")

           (setf (win-app-driver::session-data-host session) "292.168.123.45")
           (is
             (win-app-driver::get-win-app-driver-host-uri session)
             "292.168.123.45:54321")

           ; check IPv6
           (setf (win-app-driver::session-data-host session) "::1")
           (setf (win-app-driver::session-data-port session) 12345)
           (is-error
             (win-app-driver::get-win-app-driver-host-uri session)
             win-app-driver::condition-incorrect-hostname-string)

           (setf (win-app-driver::session-data-host session) "[::1]")
           (setf (win-app-driver::session-data-port session) 12345)
           (is
             (win-app-driver::get-win-app-driver-host-uri session)
             "[::1]:12345")
           )

         (is-error
           (win-app-driver::get-win-app-driver-host-uri
             (win-app-driver::make-session-data))
           win-app-driver::condition-incorrect-hostname-string)

         (is-error
           (win-app-driver::get-win-app-driver-host-uri
             (win-app-driver::make-session-data
               :host "127.0.0.1"))
           win-app-driver::condition-incorrect-port-number)

         (is-error
           (win-app-driver::get-win-app-driver-host-uri
             (win-app-driver::make-session-data
               :port 12345))
           win-app-driver::condition-incorrect-hostname-string)

         (is-error
           (win-app-driver::get-win-app-driver-host-uri
             (win-app-driver::make-session-data
               :host "127.0.0.1"
               :port 0))
           win-app-driver::condition-incorrect-port-number)

         (is
           (win-app-driver::get-win-app-driver-host-uri
             (win-app-driver::make-session-data
               :host "127.0.0.1"
               :port 1))
           "127.0.0.1:1")

         (is
           (win-app-driver::get-win-app-driver-host-uri
             (win-app-driver::make-session-data
               :host "127.0.0.1"
               :port 65535))
           "127.0.0.1:65535")

         (is-error
           (win-app-driver::get-win-app-driver-host-uri
             (win-app-driver::make-session-data
               :host "127.0.0.1"
               :port 65536))
           win-app-driver::condition-incorrect-port-number)
         )

(subtest "Testing make-desired-capabilities"
         (is
           (win-app-driver::make-desired-capabilities)
           "{\"desiredCapabilities\":{\"deviceName\":\"WindowsPC\",\"platformName\":\"Windows\"}}")
         (is
           (win-app-driver::make-desired-capabilities
             :app "C:\\Windows\\System32\\notepad.exe")
           "{\"desiredCapabilities\":{\"app\":\"C:\\\\Windows\\\\System32\\\\notepad.exe\",\"deviceName\":\"WindowsPC\",\"platformName\":\"Windows\"}}")
         (is
           (win-app-driver::make-desired-capabilities
             :app "Microsoft.WindowsCalculator_8wekyb3d8bbwe!App")
           "{\"desiredCapabilities\":{\"app\":\"Microsoft.WindowsCalculator_8wekyb3d8bbwe!App\",\"deviceName\":\"WindowsPC\",\"platformName\":\"Windows\"}}")
         (is
           (win-app-driver::make-desired-capabilities
             :app "Microsoft.MicrosoftEdge_8wekyb3d8bbwe!MicrosoftEdge"
             :app-arguments "https://github.com/Microsoft/WinAppDriver"
             :app-top-level-window "0xB822E2"
             :app-working-dir "C:\\Temp"
             :platform-name "Windows"
             :platform-version "1.0")
           "{\"desiredCapabilities\":{\"app\":\"Microsoft.MicrosoftEdge_8wekyb3d8bbwe!MicrosoftEdge\",\"appArguments\":\"https://github.com/Microsoft/WinAppDriver\",\"appTopLevelWindow\":\"0xB822E2\",\"appWorkingDir\":\"C:\\\\Temp\",\"deviceName\":\"WindowsPC\",\"platformName\":\"Windows\",\"platformVersion\":\"1.0\"}}")
         (is
           (win-app-driver::make-desired-capabilities
             :app-arguments "https://github.com/Microsoft/WinAppDriver"
             :app-top-level-window "0xB822E2"
             :app-working-dir "C:\\Temp"
             :platform-name "Windows"
             :platform-version "1.0")
           "{\"desiredCapabilities\":{\"appArguments\":\"https://github.com/Microsoft/WinAppDriver\",\"appTopLevelWindow\":\"0xB822E2\",\"appWorkingDir\":\"C:\\\\Temp\",\"deviceName\":\"WindowsPC\",\"platformName\":\"Windows\",\"platformVersion\":\"1.0\"}}")
         (is
           (win-app-driver::make-desired-capabilities
             :app-top-level-window "0xB822E2"
             :app-working-dir "C:\\Temp"
             :platform-name "Windows"
             :platform-version "1.0")
           "{\"desiredCapabilities\":{\"appTopLevelWindow\":\"0xB822E2\",\"appWorkingDir\":\"C:\\\\Temp\",\"deviceName\":\"WindowsPC\",\"platformName\":\"Windows\",\"platformVersion\":\"1.0\"}}")
         (is
           (win-app-driver::make-desired-capabilities
             :app-working-dir "C:\\Temp"
             :platform-name "Windows"
             :platform-version "1.0")
           "{\"desiredCapabilities\":{\"appWorkingDir\":\"C:\\\\Temp\",\"deviceName\":\"WindowsPC\",\"platformName\":\"Windows\",\"platformVersion\":\"1.0\"}}")
         (is
           (win-app-driver::make-desired-capabilities
             :platform-name "Windows"
             :platform-version "1.0")
           "{\"desiredCapabilities\":{\"deviceName\":\"WindowsPC\",\"platformName\":\"Windows\",\"platformVersion\":\"1.0\"}}")
         (is
           (win-app-driver::make-desired-capabilities
             :platform-version "1.0")
           "{\"desiredCapabilities\":{\"deviceName\":\"WindowsPC\",\"platformName\":\"Windows\",\"platformVersion\":\"1.0\"}}")
         (is
           (win-app-driver::make-desired-capabilities
             :app "Microsoft.MicrosoftEdge_8wekyb3d8bbwe!MicrosoftEdge"
             :app-top-level-window "0xB822E2"
             :app-working-dir "C:\\Temp"
             :platform-version "1.0")
           "{\"desiredCapabilities\":{\"app\":\"Microsoft.MicrosoftEdge_8wekyb3d8bbwe!MicrosoftEdge\",\"appTopLevelWindow\":\"0xB822E2\",\"appWorkingDir\":\"C:\\\\Temp\",\"deviceName\":\"WindowsPC\",\"platformName\":\"Windows\",\"platformVersion\":\"1.0\"}}")
         (is
           (win-app-driver::make-desired-capabilities :ms-experimental-webdriver t)
           "{\"desiredCapabilities\":{\"deviceName\":\"WindowsPC\",\"ms:experimental-webdriver\":true,\"platformName\":\"Windows\"}}")
         (is
           (win-app-driver::make-desired-capabilities :ms-wait-for-app-launch "25")
           "{\"desiredCapabilities\":{\"deviceName\":\"WindowsPC\",\"ms:waitForAppLaunch\":\"25\",\"platformName\":\"Windows\"}}"))

(subtest "Test for macro invoke-win-app-driver-api"
         (is-expand
           (win-app-driver::invoke-win-app-driver-api #'dexador:get "http://127.0.0.1:12345")
           (let ()
                (multiple-value-bind
                  ($0
                   $1
                   $2
                   $3)
                  (win-app-driver::protect-for-timeout
                    (funcall #'dexador:get
                             "http://127.0.0.1:12345"))
                  (values $0
                          $1
                          $2
                          $3))))

         (is-expand
           (win-app-driver::invoke-win-app-driver-api #'dexador:post "http://127.0.0.1:12345/session/abcd-ef01-2345" :content "{\"desiredCapabilities\":{\"app\":\"Microsoft.WindowsCalculator_8wekyb3d8bbwe!App\",\"deviceName\":\"WindowsPC\",\"platformName\":\"Windows\"}}")
           (let ()
                (multiple-value-bind
                  ($0
                   $1
                   $2
                   $3)
                  (win-app-driver::protect-for-timeout
                    (funcall #'dexador:post
                             "http://127.0.0.1:12345/session/abcd-ef01-2345"
                             "{\"desiredCapabilities\":{\"app\":\"Microsoft.WindowsCalculator_8wekyb3d8bbwe!App\",\"deviceName\":\"WindowsPC\",\"platformName\":\"Windows\"}}"))
                  (values $0
                          $1
                          $2
                          $3))))

         (is-expand
           (win-app-driver::invoke-win-app-driver-api #'dexador:delete "http://127.0.0.1:12345/session/abcd-ef01-2345")
           (let ()
                (multiple-value-bind
                  ($0
                   $1
                   $2
                   $3)
                  (win-app-driver::protect-for-timeout
                    (funcall #'dexador:delete
                             "http://127.0.0.1:12345/session/abcd-ef01-2345"))
                  (values $0
                          $1
                          $2
                          $3)))))

;(subtest "Test for macro return-win-app-driver-server-response"
;         (is-expand
;           (win-app-driver::return-win-app-driver-server-response
;             (status session))
;           (let
;             ()
;             (multiple-value-bind
;               ($0 $1 $2 $3)
;               (status session)
;               (let
;                 ()
;                 (values $0 $1 $2 $3)))))
;
;         (is-expand
;           (win-app-driver::return-win-app-driver-server-response
;             (new-session session)
;             (setf (session-data-id session) (getf (jonathan:parse json) :|sessionId|))
;             (setf (session-data-base session)
;                   (concatenate
;                     'string
;                     "/session/"
;                     (session-data-id session)
;                     "/")))
;           (let
;             ()
;             (multiple-value-bind
;               ($0 $1 $2 $3)
;               (new-session session)
;               (let
;                 ()
;                 (setf (session-data-id session) (getf (jonathan:parse json) :|sessionId|))
;                 (setf (session-data-base session)
;                       (concatenate
;                         'string
;                         "/session/"
;                         (session-data-id session)
;                         "/"))
;                 (values $0 $1 $2 $3)))))
;         )

(finalize)

