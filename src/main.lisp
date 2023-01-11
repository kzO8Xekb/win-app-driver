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
WinAppDriver serverとのsessionを確立するために必要なdesired capabilitiesを作成します。

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
  "create-session
  _/_/_/ summary _/_/_/
  This function create and return a session with WinAppDriver.

  _/_/_/ argument _/_/_/
  NIL

  _/_/_/ return value _/_/_/
  FUNCTION, Closure representing a WinAppDriver session.
  
  _/_/_/ support commands list _/_/_/
  The closure supports the following WinAppDriver commands.
  
  Command Symbol
    Arguments

  :active-element
  :back
  :button-down
    button, [optional]symbol, For this argument, specify the symbol of the mouse button to be operated. If nothing is specified, the left mouse button is the default. The possible symbols are.
                        Mouse Button Symbols       Description
                        :mouse-left-button         mouse left button.
                        :mouse-middle-button       mouse middle button.
                        :mouse-right-button        mouse right button.
  :button-up
    button, [optional]symbol, For this argument, specify the symbol of the mouse button to be operated. If nothing is specified, the left mouse button is the default. The possible symbols are.
                        Mouse Button Symbols       Description
                        :mouse-left-button         mouse left button.
                        :mouse-middle-button       mouse middle button.
                        :mouse-right-button        mouse right button.
  :click
    button, [optional]symbol, For this argument, specify the symbol of the mouse button to be operated. If nothing is specified, the left mouse button is the default. The possible symbols are.
                        Mouse Button Symbols       Description
                        :mouse-left-button         mouse left button.
                        :mouse-middle-button       mouse middle button.
                        :mouse-right-button        mouse right button.
  :close-app (for appium command) *unwork*
  :close-window
  :delete-session
  :double-click
  :element-clear
    element-id, string, This argument should specify the element ID of the element you wish to operate on.
  :element-click
    element-id, string, This argument should specify the element ID of the element you wish to operate on.
  :element-equals
    element-id1, string, Specify the element ID to be investigated in this argument.
    element-id2, string, Specify the element ID to be compared for this argument.
  :element-send-keys
    element-id, string, This argument should specify the element ID of the element you wish to operate on.
    keys, list, List of keyboard actions to be sent to the WinAppDriver server. For more information on keyboard actions, see \"https://www.w3.org/TR/webdriver1/#keyboard-actions\".
  :element-send-string (out of specification command)
    element-id, string                , This argument should specify the element ID of the element you wish to operate on.
    string,     [rest]string or symbol, Specify the string or symbol of keyboard action you want to send to the WinAppDriver server.
  :find-element
    selector, symbol, This argument specify how to search for element ID.
                        Selector                   Description
                        :accessibility-id-selector finding element accessibility id.
                        :accessibility-id          finding element accessibility id.
                        :automation-id-selector    finding element accessibility id.
                        :automation-id             finding element accessibility id.
                        :class-name-selector       finding element class name.
                        :class-name                finding element class name.
                        :name-selector             finding element name.
                        :name                      finding element name.
                        :xpath-selector            finding element xpath.
                        :xpath                     finding element xpath.
    value,    string, This argument should be a string to identify the element.
  :find-element-from-element
    element-id, string, This argument should specify the element ID of the element you wish to operate on.
    selector, symbol, This argument specify how to search for element ID.
                        Selector                   Description
                        :accessibility-id-selector finding element accessibility id.
                        :accessibility-id          finding element accessibility id.
                        :automation-id-selector    finding element accessibility id.
                        :automation-id             finding element accessibility id.
                        :class-name-selector       finding element class name.
                        :class-name                finding element class name.
                        :name-selector             finding element name.
                        :name                      finding element name.
                        :xpath-selector            finding element xpath.
                        :xpath                     finding element xpath.
    value,    string, This argument should be a string to identify the element.
  :find-elements-from-element
    element-id, string, This argument should specify the element ID of the element you wish to operate on.
    selector, symbol, This argument specify how to search for element IDs.
                        Selector                   Description
                        :accessibility-id-selector finding element accessibility id.
                        :accessibility-id          finding element accessibility id.
                        :automation-id-selector    finding element accessibility id.
                        :automation-id             finding element accessibility id.
                        :class-name-selector       finding element class name.
                        :class-name                finding element class name.
                        :name-selector             finding element name.
                        :name                      finding element name.
                        :xpath-selector            finding element xpath.
                        :xpath                     finding element xpath.
    value,    string, This argument should be a string to identify the elements.
  :find-elements
    selector, symbol, This argument specify how to search for element IDs.
                        Selector                   Description
                        :accessibility-id-selector finding element accessibility id.
                        :accessibility-id          finding element accessibility id.
                        :automation-id-selector    finding element accessibility id.
                        :automation-id             finding element accessibility id.
                        :class-name-selector       finding element class name.
                        :class-name                finding element class name.
                        :name-selector             finding element name.
                        :name                      finding element name.
                        :xpath-selector            finding element xpath.
                        :xpath                     finding element xpath.
    value,    string, This argument should be a string to identify the elements.
  :forward
  :get-element-attribute
    element-id,     string, This argument should specify the element ID of the element you wish to operate on.
    attribute-name, string, This argument should be the name of the attribute.
  :get-element-location
    element-id, string, This argument should specify the element ID of the element you wish to operate on.
  :get-element-location-in-view
    element-id, string, This argument should specify the element ID of the element you wish to operate on.
  :get-element-name
    element-id, string, This argument should specify the element ID of the element you wish to operate on.
  :get-element-size
    element-id, string, This argument should specify the element ID of the element you wish to operate on.
  :get-element-text
    element-id, string, This argument should specify the element ID of the element you wish to operate on.
  :get-sessions
  :get-source
  :get-title
  :get-window-handle
  :get-window-handles
  :get-window-position-with-window-handle
    window-handle, string, This argument should specify the handle of the window you wish to operate on.
  :get-window-size
  :get-window-size-with-window-handle
    window-handle, string, This argument should specify the handle of the window you wish to operate on.
  :is-element-displayed
    element-id, string, This argument should specify the element ID of the element to be investigated.
  :is-element-enabled
    element-id, string, This argument should specify the element ID of the element to be investigated.
  :is-element-selected
    element-id, string, This argument should specify the element ID of the element to be investigated.
  :launch-app (for appium command) *unwork*
  :location *unwork*
  :maximize-window
  :move-to
    arg1(element-id), string,  This argument should specify element-id of the element becoming the moving starting point.
    arg2(xoffset),    integer, This argument should specify the x-coordinate to be moved to.
    arg3(yoffset),    integer, This argument should specify the y-coordinate to be moved to.
       or
    arg1(xoffset),    integer, This argument should specify the x-coordinate to be moved to.
    arg2(yoffset),    integer, This argument should specify the y-coordinate to be moved to.
  :new-session
    app,                  [key]string, Application identifier or executable full path. ex. \"Microsoft.MicrosoftEdge_8wekyb3d8bbwe!MicrosoftEdge\"
    app-arguments,        [key]string, Application launch arguments. ex. \"https://github.com/Microsoft/WinAppDriver\"
                                       When specifying the executable full path, \"\\\" should be converted to \"/\". This is bug sorry.
    app-top-level-window, [key]string, Existing application top level window to attach to. ex. \"0xB822E2\"
    app-working-dir,      [key]string, Application working directory (Classic apps only). ex. \"C:\\Temp\"
    device-name,          [key]string, Application working device type name. ex. \"WindowsPC\"
    platform-name,        [key]string, Target platform name. ex. \"Windows\"
    platform-version,     [key]string, Target platform version. ex. \"1.0\"
  :orientation
  :send-keys
    keys, list, List of keyboard actions to be sent to the WinAppDriver server. For more information on keyboard actions, see \"https://www.w3.org/TR/webdriver1/#keyboard-actions\".
  :send-string (out of specification command)
    string, [rest]string, Specify the string or symbol of keyboard action you want to send to the WinAppDriver server.
  :set-timeouts
    implicit, [key]integer, This argument should specify the new implicit wait timeout.
  :set-window-position-with-window-handle
    window-handle, string,  This argument should specify the handle of the window you wish to operate on.
    x,             integer, This argument should specify the new x-axis value.
    y,             integer, This argument should specify the new y-axis value.
  :set-window-size
    width,  integer, This argument should specify the new width value.
    height, integer, This argument should specify the new height value.
  :set-window-size-with-window-handle
    window-handle, string,  This argument should specify the handle of the window you wish to operate on.
    width,         integer, This argument should specify the new width value.
    height,        integer, This argument should specify the new height value.
  :status
  :take-element-screenshot
  :take-screenshot
  :touch-click
    element-id, string, This argument should specify the element ID of the element you wish to operate on.
  :touch-double-click
    element-id, string, This argument should specify the element ID of the element you wish to operate on.
  :touch-down
    x, integer, This argument should specify the x-coordinate of the operating position.
    y, integer, This argument should specify the y-coordinate of the operating position.
  :touch-flick
    arg1(xspeed),     integer, This argument should specify the flick speed in the x-axis direction.
    arg2(yspeed),     integer, This argument should specify the flick speed in the y-axis direction.
    or
    arg1(element-id), string,  This argument should specify element-id of the element becoming the moving starting point.
    arg2(xoffset),    integer, This argument should specify the x-coordinate to be moved to.
    arg3(yoffset),    integer, This argument should specify the y-coordinate to be moved to.
    arg4(speed),      integer, This argument should specify the flick speed.
  :touch-long-click
    element-id, string, This argument should specify the element ID of the element you wish to operate on.
  :touch-move
    x, integer, This argument should specify the x-coordinate to be moved to.
    y, integer, This argument should specify the y-coordinate to be moved to.
  :touch-scroll
    arg1(xoffset),    integer, This argument should specify the x-coordinate to be moved to.
    arg2(yoffset),    integer, This argument should specify the y-coordinate to be moved to.
    or
    arg1(element-id), string,  This argument should specify element-id of the element becoming the scrolling starting point.
    arg2(xoffset),    integer, This argument should specify the x-coordinate to be moved to.
    arg3(yoffset),    integer, This argument should specify the y-coordinate to be moved to.
  :touch-up
    x, integer, This argument should specify the x-coordinate of the operating position.
    y, integer, This argument should specify the y-coordinate of the operating position.
  :window-maximize-with-window-handle
    window-handle, string,  This argument should specify the handle of the window you wish to operate on.
  "
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
                                         (session-data-id session)))))
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
                 (:button-down (&optional (button :mouse-left-button)) 
                               (return-win-app-driver-server-response 
                                 (button-down session button)))
                 (:button-up (&optional (button :mouse-left-button)) 
                             (return-win-app-driver-server-response 
                               (button-up session button)))
                 (:click (&optional (button :mouse-left-button))
                         (return-win-app-driver-server-response 
                           (click session button)))
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
                 (:is-element-displayed (element-id) 
                                        (return-win-app-driver-server-response 
                                          (is-element-displayed session element-id)))
                 (:find-element-from-element (element-id selector value) 
                                             (return-win-app-driver-server-response 
                                               (find-element-from-element session element-id selector value)))
                 (:find-elements-from-element (element-id selector value) 
                                              (return-win-app-driver-server-response 
                                                (find-elements-from-element session element-id selector value)))
                 (:is-element-enabled (element-id) 
                                      (return-win-app-driver-server-response 
                                        (is-element-enabled session element-id)))
                 (:element-equals (element-id1 element-id2) 
                                  (return-win-app-driver-server-response 
                                    (element-equals session element-id1 element-id2)))
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
                 (:element-send-keys (element-id keys)  ; https://www.w3.org/TR/webdriver/#element-send-keys
                                     (return-win-app-driver-server-response 
                                       (element-send-keys session element-id keys)))
                 (:element-send-string (element-id &rest string) ; non-specific function.
                                       (return-win-app-driver-server-response 
                                         (element-send-string session element-id string)))
                 (:forward () 
                           (return-win-app-driver-server-response 
                             (forward session)))
                 (:send-keys (keys)
                             (return-win-app-driver-server-response 
                               (send-keys session keys)))
                 (:send-string (&rest string) ; non-specific function.
                               (return-win-app-driver-server-response 
                                 (send-string session string)))
                 (:location () 
                            (return-win-app-driver-server-response 
                              (location session)))
                 (:move-to (arg1 arg2 &optional (arg3 nil))
                           (return-win-app-driver-server-response 
                             (move-to session arg1 arg2 arg3)))
                 (:orientation () 
                               (return-win-app-driver-server-response 
                                 (orientation session)))
                 (:take-screenshot () 
                                   (return-win-app-driver-server-response 
                                     (take-screenshot session)))
                 (:get-source ()
                              (return-win-app-driver-server-response 
                                (get-source session)))
                 (:set-timeouts (&key implicit) 
                                (return-win-app-driver-server-response 
                                  (set-timeouts session
                                                :implicit implicit)))
                 (:get-title ()
                             (return-win-app-driver-server-response 
                               (get-title session)))
                 (:touch-click (element-id) 
                               (return-win-app-driver-server-response 
                                 (touch-click session element-id)))
                 (:touch-double-click (element-id) 
                                      (return-win-app-driver-server-response 
                                        (touch-double-click session element-id)))
                 (:touch-down (x y)
                              (return-win-app-driver-server-response 
                                (touch-down session x y)))
                 (:touch-flick (arg1 arg2 &optional (arg3 nil) (arg4 nil))
                               (return-win-app-driver-server-response
                                 (touch-flick session arg1 arg2 arg3 arg4)))
                 (:touch-long-click (element-id) 
                                    (return-win-app-driver-server-response 
                                      (touch-long-click session element-id)))
                 (:touch-move (x y) 
                              (return-win-app-driver-server-response 
                                (touch-move session x y)))
                 (:touch-scroll (arg1 arg2 &optional (arg3 nil)) 
                                (return-win-app-driver-server-response 
                                  (touch-scroll session arg1 arg2 arg3)))
                 (:touch-up (x y) 
                            (return-win-app-driver-server-response 
                              (touch-up session x y)))
                 (:close-window () 
                                (return-win-app-driver-server-response 
                                  (close-window session)))
                 (:maximize-window () 
                                   (return-win-app-driver-server-response 
                                     (maximize-window session)))
                 (:get-window-size () 
                                   (return-win-app-driver-server-response 
                                     (get-window-size session)))
                 (:set-window-size (width height)
                                   (return-win-app-driver-server-response
                                     (set-window-size session width height)))
                 (:get-window-size-with-window-handle (window-handle) 
                                                      (return-win-app-driver-server-response 
                                                        (get-window-size-with-window-handle session window-handle)))
                 (:set-window-size-with-window-handle (window-handle width height)
                                                      (return-win-app-driver-server-response
                                                        (set-window-size-with-window-handle session window-handle width height)))
                 (:get-window-position-with-window-handle (window-handle) 
                                                          (return-win-app-driver-server-response 
                                                            (get-window-position-with-window-handle session window-handle)))
                 (:set-window-position-with-window-handle (window-handle x y) 
                                                          (return-win-app-driver-server-response
                                                            (set-window-position-with-window-handle session window-handle x y)))
                 (:window-maximize-with-window-handle (window-handle)
                                                      (return-win-app-driver-server-response 
                                                        (window-maximize-with-window-handle session window-handle)))
                 (:get-window-handle ()
                                     (return-win-app-driver-server-response 
                                       (get-window-handle session)))
                 (:get-window-handles ()
                                      (return-win-app-driver-server-response
                                        (get-window-handles session))))
          self (lambda (&rest args)
                 (apply impl args)))))))

