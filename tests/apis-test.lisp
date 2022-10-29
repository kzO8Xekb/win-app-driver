(in-package :win-app-driver/tests)

;; NOTE: To run this test file, execute `(asdf:test-system :win-app-driver)' in your Lisp.

(defparameter *win-app-driver-host* "localhost")
(defparameter *win-app-driver-port* 12345)

(subtest "Testing hostname-p"
         (is-values
           (win-app-driver::hostname-p "localhost")
           '(0 9)))

(subtest "Testing correct-host-identifier-p"
         (is-values
           (win-app-driver::correct-host-identifier-p "localhost")
           '("localhost" t))
         (is-values
           (win-app-driver::correct-host-identifier-p "0.0.0.0")
           '("0.0.0.0" t))
         (is-values
           (win-app-driver::correct-host-identifier-p "127.0.0.1")
           '("127.0.0.1" t))
         (is-values
           (win-app-driver::correct-host-identifier-p "192.168.123.45")
           '("192.168.123.45" t))
         (is-values
           (win-app-driver::correct-host-identifier-p "255.255.255.255")
           '("255.255.255.255" t))
         (is-values
           (win-app-driver::correct-host-identifier-p "256.255.255.255")
           '("256.255.255.255" t))
         (is-values
           (win-app-driver::correct-host-identifier-p "255.256.255.255")
           '("255.256.255.255" t))
         (is-values
           (win-app-driver::correct-host-identifier-p "255.255.256.255")
           '("255.255.256.255" t))
         (is-values
           (win-app-driver::correct-host-identifier-p "255.255.255.256")
           '("255.255.255.256" t))
         (is-values
           (win-app-driver::correct-host-identifier-p "128.128.128.128")
           '("128.128.128.128" t))
         (is-values
           (win-app-driver::correct-host-identifier-p "[::1]")
           '("[::1]" t)))

(subtest "Testing get-win-app-driver-host-uri"
         (let
           ((session (win-app-driver::create-session)))
           ; check host name
           (funcall session :pandoric-set 'win-app-driver::host "localhost")
           (funcall session :pandoric-set 'win-app-driver::port 12345)
           (is
             (win-app-driver::get-win-app-driver-host-uri session)
             "localhost:12345")

           (funcall session :pandoric-set 'win-app-driver::host "foo.bar.baz")
           (is
             (win-app-driver::get-win-app-driver-host-uri session)
             "foo.bar.baz:12345")

           ; check IPv4 address
           (funcall session :pandoric-set 'win-app-driver::host "127.0.0.1")
           (funcall session :pandoric-set 'win-app-driver::port 54321)
           (is
             (win-app-driver::get-win-app-driver-host-uri session)
             "127.0.0.1:54321")

           (funcall session :pandoric-set 'win-app-driver::host "192.168.123.45")
           (is
             (win-app-driver::get-win-app-driver-host-uri session)
             "192.168.123.45:54321")

           (funcall session :pandoric-set 'win-app-driver::host "292.168.123.45")
           (is
             (win-app-driver::get-win-app-driver-host-uri session)
             "292.168.123.45:54321")

           ; check IPv6
           (funcall session :pandoric-set 'win-app-driver::host "::1")
           (funcall session :pandoric-set 'win-app-driver::port 12345)
           (is-error
             (win-app-driver::get-win-app-driver-host-uri session)
             win-app-driver::condition-incorrect-hostname-string)

           (funcall session :pandoric-set 'win-app-driver::host "[::1]")
           (funcall session :pandoric-set 'win-app-driver::port 12345)
           (is
             (win-app-driver::get-win-app-driver-host-uri session)
             "[::1]:12345")
           )

         (let
           ((session (win-app-driver::create-session)))
           (is-error
             (win-app-driver::get-win-app-driver-host-uri session)
             win-app-driver::condition-incorrect-hostname-string)) 

         (let
           ((session (win-app-driver::create-session)))
           (funcall session :pandoric-set 'win-app-driver::host "127.0.0.1")
           (is-error
             (win-app-driver::get-win-app-driver-host-uri session)
             win-app-driver::condition-incorrect-port-number))

         (let
           ((session (win-app-driver::create-session)))
           (funcall session :pandoric-set 'win-app-driver::port 12345)
           (is-error
             (win-app-driver::get-win-app-driver-host-uri session)
             win-app-driver::condition-incorrect-hostname-string))
         )

(subtest "Testing generate-desired-capabilities"
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
         )

;(subtest "Test for macro expand-with-session-body"
;         (is-expand
;           (expand-with-session-body
;             wds
;             (wds
;               :maximize-window)
;             (wds
;               :navigate-to
;               url)
;             (foo
;               (wds
;                 :get-element-text
;                 (wds
;                   :get-session-id
;                   :xpath
;                   "//bar"))))
;           (progn
;             (funcall
;               wds
;               :maximize-window)
;             (funcall
;               wds
;               :navigate-to
;               url)
;             (foo
;               (funcall
;                 wds
;                 :get-element-text
;                 (funcall
;                   wds
;                   :get-session-id
;                   :xpath
;                   "//bar"))))))
;
;(subtest "test for macro with-session"
;         (is-expand
;           (with-session
;             (wds)
;             nil)
;           (let
;             ((wds (win-app-driver::create-session))
;              ($0 (let-over-lambda:aif
;                    nil let-over-lambda:it
;                    "localhost"))
;              ($1 (let-over-lambda:aif
;                    nil let-over-lambda:it
;                    9515)))
;             (unwind-protect
;               (progn
;                 (funcall wds
;                          :new-session
;                          :host $0
;                          :port $1)
;                 (win-app-driver::expand-with-session-body
;                   wds
;                   nil))
;               (progn
;                 (funcall wds
;                          :delete-session)))))
;
;         (is-expand
;           (with-session
;             (wds)
;             (wds :maximize-window))
;           (let
;             ((wds (win-app-driver::create-session))
;              ($0 (let-over-lambda:aif
;                    nil let-over-lambda:it
;                    "localhost"))
;              ($1 (let-over-lambda:aif
;                    nil let-over-lambda:it
;                    9515)))
;             (unwind-protect
;               (progn
;                 (funcall wds
;                          :new-session
;                          :host $0
;                          :port $1)
;                 (win-app-driver::expand-with-session-body
;                   wds (wds :maximize-window)))
;               (progn
;                 (funcall wds
;                          :delete-session)))))
;
;         (is-expand
;           (with-session
;             (wds)
;             (wds :maximize-window)
;             (wds :navigate-to url)
;             (foo (wds :get-element-text (wds :get-session-id :xpath "//bar"))))
;           (let
;             ((wds (win-app-driver::create-session))
;              ($0 (let-over-lambda:aif
;                    nil let-over-lambda:it
;                    "localhost"))
;              ($1 (let-over-lambda:aif
;                    nil let-over-lambda:it
;                    9515)))
;             (unwind-protect
;               (progn
;                 (funcall wds
;                          :new-session
;                          :host $0
;                          :port $1)
;                 (win-app-driver::expand-with-session-body
;                   wds
;                   (wds :maximize-window)
;                   (wds :navigate-to url)
;                   (foo (wds :get-element-text (wds :get-session-id :xpath "//bar")))))
;               (progn
;                 (funcall wds
;                          :delete-session)))))
;
;         (is-expand
;           (with-session
;             (wds :port 12345 :host "127.0.0.1")
;             (wds :maximize-window)
;             (wds :navigate-to url)
;             (foo (wds :get-element-text (wds :get-session-id :xpath "//bar"))))
;           (let
;             ((wds (win-app-driver::create-session))
;              ($0 (let-over-lambda:aif
;                    "127.0.0.1" let-over-lambda:it
;                    "localhost"))
;              ($1 (let-over-lambda:aif
;                    12345 let-over-lambda:it
;                    9515)))
;             (unwind-protect
;               (progn
;                 (funcall wds
;                          :new-session
;                          :host $0
;                          :port $1)
;                 (win-app-driver::expand-with-session-body
;                   wds
;                   (wds :maximize-window)
;                   (wds :navigate-to url)
;                   (foo (wds :get-element-text (wds :get-session-id :xpath "//bar")))))
;               (progn
;                 (funcall wds
;                          :delete-session)))))
;
;         (is-expand
;           (with-session
;             (wds :host "127.0.0.1")
;             (wds :maximize-window)
;             (wds :navigate-to url)
;             (foo (wds :get-element-text (wds :get-session-id :xpath "//bar"))))
;           (let
;             ((wds (win-app-driver::create-session))
;              ($0 (let-over-lambda:aif
;                    "127.0.0.1" let-over-lambda:it
;                    "localhost"))
;              ($1 (let-over-lambda:aif
;                    nil let-over-lambda:it
;                    9515)))
;             (unwind-protect
;               (progn
;                 (funcall wds
;                          :new-session
;                          :host $0
;                          :port $1)
;                 (win-app-driver::expand-with-session-body
;                   wds
;                   (wds :maximize-window)
;                   (wds :navigate-to url)
;                   (foo (wds :get-element-text (wds :get-session-id :xpath "//bar")))))
;               (progn
;                 (funcall wds
;                          :delete-session)))))
;
;         (is-expand
;           (with-session
;             (wds :port 12345)
;             (wds :maximize-window)
;             (wds :navigate-to url)
;             (foo (wds :get-element-text (wds :get-session-id :xpath "//bar"))))
;           (let
;             ((wds (win-app-driver::create-session))
;              ($0 (let-over-lambda:aif
;                    nil let-over-lambda:it
;                    "localhost"))
;              ($1 (let-over-lambda:aif
;                    12345 let-over-lambda:it
;                    9515)))
;             (unwind-protect
;               (progn
;                 (funcall wds
;                          :new-session
;                          :host $0
;                          :port $1)
;                 (win-app-driver::expand-with-session-body
;                   wds
;                   (wds :maximize-window)
;                   (wds :navigate-to url)
;                   (foo (wds :get-element-text (wds :get-session-id :xpath "//bar")))))
;               (progn
;                 (funcall wds
;                          :delete-session)))))
;
;         (is-expand
;           (with-session
;             (wds :host "127.0.0.1" :port 12345)
;             (wds :maximize-window)
;             (wds :navigate-to url)
;             (foo (wds :get-element-text (wds :get-session-id :xpath "//bar"))))
;           (let
;             ((wds (win-app-driver::create-session))
;              ($0 (let-over-lambda:aif
;                    "127.0.0.1" let-over-lambda:it
;                    "localhost"))
;              ($1 (let-over-lambda:aif
;                    12345 let-over-lambda:it
;                    9515)))
;             (unwind-protect
;               (progn
;                 (funcall wds
;                          :new-session
;                          :host $0
;                          :port $1)
;                 (win-app-driver::expand-with-session-body
;                   wds
;                   (wds :maximize-window)
;                   (wds :navigate-to url)
;                   (foo (wds :get-element-text (wds :get-session-id :xpath "//bar")))))
;               (progn
;                 (funcall wds
;                          :delete-session)))))
;
;         (is-expand
;           (with-session
;             (wds :host "127.0.0.1"
;                  :port 12345
;                  :options '(:|args| ("start-maximized" "user-data-dir=/tmp/temp_profile")
;                             :|binary| "/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge"))
;             (wds :maximize-window)
;             (wds :navigate-to url)
;             (foo (wds :get-element-text (wds :get-session-id :xpath "//bar"))))
;           (let
;             ((wds (win-app-driver::create-session))
;              ($0 (let-over-lambda:aif
;                    "127.0.0.1" let-over-lambda:it
;                    "localhost"))
;              ($1 (let-over-lambda:aif
;                    12345 let-over-lambda:it
;                    9515))
;              ($2 (let-over-lambda:aif
;                    '(:|args|
;                       ("start-maximized"
;                        "user-data-dir=/tmp/temp_profile")
;                       :|binary|
;                       "/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge")
;                    let-over-lambda:it
;                    nil)))
;             (unwind-protect
;               (progn
;                 (funcall wds
;                          :new-session
;                          :host $0
;                          :port $1
;                          :options $2)
;                 (win-app-driver::expand-with-session-body
;                   wds
;                   (wds :maximize-window)
;                   (wds :navigate-to url)
;                   (foo (wds :get-element-text (wds :get-session-id :xpath "//bar")))))
;               (progn
;                 (funcall wds
;                          :delete-session)))))
;
;         )

;(with-session
;  (wds :host *win-app-driver-host* :port *win-app-driver-port*)
;  (subtest "Testing get-current-url and navigate-to"
;           (is
;             (wds :get-current-url)
;             "data:,")
;
;           (wds :navigate-to "https://hotel.testplanisphere.dev/ja/")
;
;           (is
;             (wds :get-current-url)
;             "https://hotel.testplanisphere.dev/ja/")))

(finalize)

