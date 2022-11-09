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
       (with-gensyms
         ($json $status-code $hash $quri)
         `(multiple-value-bind
            ($json $status-code $hash $quri)
            ,expr
            (let ()
              ,@body
              (values $json $status-code $hash $quri))))))
    (lol:pandoriclet
      ((session nil))
      (let
        (impl self)
        (setq
          impl (lol:dlambda
                 (:status ()
                          (return-win-app-driver-server-response
                            (status session)))
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
                 (:get-sessions ()
                                (return-win-app-driver-server-response
                                  (get-sessions session)))
                 (:delete-session ()
                                  (cond
                                    (session
                                      (return-win-app-driver-server-response
                                        (delete-session session)
                                        (setf session nil)))
                                    (t (values nil nil nil nil))))
                 (:launch-app () 
                              (return-win-app-driver-server-response 
                                (launch-app session)))
                 (:close-app () 
                             (return-win-app-driver-server-response 
                               (close-app session)))
                 (:back () 
                        (return-win-app-driver-server-response 
                          (back session)))
                 (:button-down () 
                               (return-win-app-driver-server-response 
                                 (button-down session)))
                 (:button-up () 
                             (return-win-app-driver-server-response 
                               (button-up session)))
                 (:click () 
                         (return-win-app-driver-server-response 
                           (click session)))
                 (:double-click () 
                                (return-win-app-driver-server-response 
                                  (double-click session)))
                 (:find-element (selector value) 
                                (return-win-app-driver-server-response 
                                  (find-element session selector value)))
                 (:find-elements (selector value) 
                                 (return-win-app-driver-server-response 
                                   (find-elements session selector value)))
                 (:active-element () 
                                  (return-win-app-driver-server-response 
                                    (active-element session)))
                 (:get-element-attribute (element-id attribute-name) 
                                         (return-win-app-driver-server-response 
                                           (get-element-attribute session element-id attribute-name)))
                 (:element-clear (element-id) 
                                 (return-win-app-driver-server-response 
                                   (element-clear session element-id)))
                 (:element-click (element-id) 
                                 (return-win-app-driver-server-response 
                                   (element-click session element-id)))
                 (:element-displayed (element-id) 
                                     (return-win-app-driver-server-response 
                                       (element-displayed session element-id)))
                 (:find-element-from-element (element-id selector value) 
                                             (return-win-app-driver-server-response 
                                               (find-element-from-element session element-id selector value)))
                 (:find-element-from-elements (element-id selector value) 
                                              (return-win-app-driver-server-response 
                                                (find-element-from-elements session element-id selector value)))
                 (:is-element-enabled (element-id) 
                                      (return-win-app-driver-server-response 
                                        (is-element-enabled session element-id)))
                 (:element-equals (element-id) 
                                  (return-win-app-driver-server-response 
                                    (element-equals session element-id)))
                 (:get-element-location (element-id) 
                                        (return-win-app-driver-server-response 
                                          (get-element-location session element-id)))
                 (:get-element-location-in-view (element-id) 
                                                (return-win-app-driver-server-response 
                                                  (get-element-location-in-view session element-id)))
                 (:get-element-name (element-id) 
                                    (return-win-app-driver-server-response 
                                      (get-element-name session element-id)))
                 (:take-element-screenshot (element-id) 
                                           (return-win-app-driver-server-response 
                                             (take-element-screenshot session element-id)))
                 (:is-element-selected (element-id) 
                                       (return-win-app-driver-server-response 
                                         (is-element-selected session element-id)))
                 (:get-element-size (element-id) 
                                    (return-win-app-driver-server-response 
                                      (get-element-size session element-id)))
                 (:get-element-text (element-id) 
                                    (return-win-app-driver-server-response 
                                      (get-element-text session element-id)))
                 (:get-element-value (element-id) 
                                     (return-win-app-driver-server-response 
                                       (get-element-value session element-id)))
                 (:forward () 
                           (return-win-app-driver-server-response 
                             (forward session)))
                 (:send-keys (keys) ; 形だけの実装
                             (return-win-app-driver-server-response 
                               (send-keys session keys)))
                 (:send-string (string) ; 仕様外関数
                             (return-win-app-driver-server-response 
                               (send-string session string)))
                 (:location () 
                            (return-win-app-driver-server-response 
                              (location session)))
                 (:move-to () 
                           (return-win-app-driver-server-response 
                             (move-to session)))
                 (:orientation () 
                               (return-win-app-driver-server-response 
                                 (orientation session)))
                 (:take-screenshot () 
                                   (return-win-app-driver-server-response 
                                     (take-screenshot session)))
                 (:get-source () 
                              (return-win-app-driver-server-response 
                                (get-source session)))
                 (:set-timeouts () 
                                (return-win-app-driver-server-response 
                                  (set-timeouts session)))
                 (:get-title () 
                             (return-win-app-driver-server-response 
                               (get-title session)))
                 (:touch-click () 
                               (return-win-app-driver-server-response 
                                 (touch-click session)))
                 (:touch-doubleclick () 
                                     (return-win-app-driver-server-response 
                                       (touch-doubleclick session)))
                 (:touch-down () 
                              (return-win-app-driver-server-response 
                                (touch-down session)))
                 (:touch-flick () 
                               (return-win-app-driver-server-response 
                                 (touch-flick session)))
                 (:touch-longclick () 
                                   (return-win-app-driver-server-response 
                                     (touch-longclick session)))
                 (:touch-move () 
                              (return-win-app-driver-server-response 
                                (touch-move session)))
                 (:touch-scroll () 
                                (return-win-app-driver-server-response 
                                  (touch-scroll session)))
                 (:touch-up () 
                            (return-win-app-driver-server-response 
                              (touch-up session)))
                 (:close-window () 
                                (return-win-app-driver-server-response 
                                  (close-window session)))
                 (:switch-to-window () 
                                    (return-win-app-driver-server-response 
                                      (switch-to-window session)))
                 (:maximize-window () 
                                   (return-win-app-driver-server-response 
                                     (maximize-window session)))
                 (:get-window-size () 
                                   (return-win-app-driver-server-response 
                                     (get-window-size session)))
                 (:set-window-size () 
                                   (return-win-app-driver-server-response 
                                     (set-window-size session)))
                 (:get-window-size-with-window-handle (window-handle) 
                                                      (return-win-app-driver-server-response 
                                                        (get-window-size-with-window-handle session window-handle)))
                 (:set-window-size-with-window-handle (window-handle) 
                                                      (return-win-app-driver-server-response 
                                                        (set-window-size-with-window-handle session window-handle)))
                 (:get-window-position-with-window-handle (window-handle) 
                                                          (return-win-app-driver-server-response 
                                                            (get-window-position-with-window-handle session window-handle)))
                 (:set-window-position-with-window-handle (window-handle) 
                                                          (return-win-app-driver-server-response 
                                                            (set-window-position-with-window-handle session window-handle)))
                 (:window-maximize-with-window-handle (window-handle)
                                                      (return-win-app-driver-server-response 
                                                        (window-maximize-with-window-handle session window-handle)))
                 (:get-window-handle ()  ; ToDo 形だけの実装
                                     (return-win-app-driver-server-response 
                                       (get-window-handle session)))
                 (:get-window-handles () ; ToDo 形だけの実装
                                      (return-win-app-driver-server-response
                                        (get-window-handles session))))
          self (lambda (&rest args)
                 (apply impl args)))))))

