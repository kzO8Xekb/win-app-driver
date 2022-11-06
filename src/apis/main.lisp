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

;(lol:defmacro! return-win-app-driver-server-response (expr &body body)
; `(multiple-value-bind
;    (,g!json ,g!status-code ,g!hash ,g!quri)
;    ,expr
;    (let ()
;      ,@body
;      (values ,g!json ,g!status-code ,g!hash ,g!quri))))

(defun create-session ()
  (macrolet
    ((return-win-app-driver-server-response (expr &body body)
                                            `(multiple-value-bind
                                               ($json $status-code $hash $quri)
                                               ,expr
                                               (let ()
                                                 ,@body
                                                 (values $json $status-code $hash $quri)))))
    (lol:pandoriclet
      ((session nil))
      (let
        (impl self)
        (setq
          impl (lol:dlambda
                 (:status () (return-win-app-driver-server-response (status session)))
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
                               (return-win-app-driver-server-response
                                 (new-session session)
                                 (setf (session-data-id session) (getf (jonathan:parse $json) :|sessionId|))
                                 (setf (session-data-base session)
                                       (concatenate
                                         'string
                                         "/session/"
                                         (session-data-id session)
                                         "/"))))
                 (:delete-session ()
                                  (cond
                                    (session
                                      (return-win-app-driver-server-response
                                        (delete-session session)
                                        (setf session nil)))
                                    (t (values nil nil nil nil)))))
          self (lambda (&rest args)
                 (apply impl args)))))))

