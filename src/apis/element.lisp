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

(defparameter *locator-strategies-table* (alexandria:plist-hash-table
                                           `(:accessibility-id-selector "accessibility id"
                                             :accessibility-id          "accessibility id"
                                             :automation-id-selector    "accessibility id"
                                             :automation-id             "accessibility id"
                                             :class-name-selector       "class name"      
                                             :class-name                "class name"      
                                             :name-selector             "name"            
                                             :name                      "name"            
                                             :xpath-selector            "xpath"           
                                             :xpath                     "xpath"           )))

(declaim (inline locator-strategies))
(defun locator-strategies (selector)
  "引数で指定したlocatorを表すシンボルに応じて，JSONで使用すべき文字列を返します。"
  (gethash
    selector
    *locator-strategies-table*))

(declaim (inline generate-element-string))
(defun generate-element-string (using value)
  "第1引数で，locatorの種類を指定し，第2引数でWinAppDriver Serverに渡すvalueを指定すると，JSONの文字列を生成して返します。"
  (concatenate
    'string
    "{"
    "\"using\":\"" using "\","
    "\"value\":\"" value "\""
    "}"))

(declaim (inline generate-find-element-json-string))
(defun generate-find-element-json-string (selector value)
  "generate-find-element-json-string
  _/_/_/ 概要 _/_/_/
  find-element系関数で使用するJSON文字列を作成します。

  _/_/_/ 引数 _/_/_/
  selector, symbol: element位置の特定方法を，シンボルで指定します。
  symbol             Matched Attribute
  :accessibility-id  AutomationId
  :class-name        ClassName
  :name              Name
  :xpath             XPath String
  value,    string: 検索するelementを特定するための文字列を指定します。Accessibility Insights for WindowsやUI Recordersで調査した値を指定します。

  _/_/_/ 返り値 _/_/_/
  string: WinAppDriver Serverに送信するJSONフォーマットの文字列。"
  (multiple-value-bind
    (using success)
    (locator-strategies selector)
    (make-error-condition-if
      (complement (lambda () success))
      'condition-incorrect-arguments
      'generate-find-element-json-string)
    (generate-element-string
      using
      value)))

(defun get-element-id (response)
  (getf (response-accessor response :|value|) :ELEMENT))

;POST 	/session/:sessionId/element
(defun find-element (session selector value)
  (send-command
    session
    :post
    ((session-data-base session) "/element")
    (generate-find-element-json-string
      selector
      value)))

;POST 	/session/:sessionId/elements
(defun find-elements (session selector value) 
  (send-command
    session
    :post
    ((session-data-base session) "/elements")
    (generate-find-element-json-string
      selector
      value)))

;POST 	/session/:sessionId/element/active
(defun active-element (session)
  (send-command
    session
    :post
    ((session-data-base session) "/element/active")))

;GET 	/session/:sessionId/element/:id/attribute/:name
(defun get-element-attribute (session element-id attribute-name)
  (send-command
    session
    :get
    ((session-data-base session) "/element/" element-id "/attribute/" attribute-name)))

;POST 	/session/:sessionId/element/:id/clear
(defun element-clear (session element-id)
  (send-command
    session
    :post
    ((session-data-base session) "/element/" element-id "/clear")))

;POST 	/session/:sessionId/element/:id/click
(defun element-click (session element-id)
  (send-command
    session
    :post
    ((session-data-base session) "/element/" element-id "/click")))

;GET 	/session/:sessionId/element/:id/displayed
(defun is-element-displayed (session element-id)
  (send-command
    session
    :get
    ((session-data-base session) "/element/" element-id "/displayed")))

;GET 	/session/:sessionId/element/:id/element
(defun find-element-from-element (session element-id selector value)
  (send-command
    session
    :get
    ((session-data-base session) "/element/" element-id "/element")
    (generate-find-element-json-string
      selector
      value)))

;GET 	/session/:sessionId/element/:id/elements
(defun find-element-from-elements (session element-id selector value)
  (send-command
    session
    :get
    ((session-data-base session) "/element/" element-id "/elements")
    (generate-find-element-json-string
      selector
      value)))

;GET 	/session/:sessionId/element/:id/enabled
(defun is-element-enabled (session element-id)
  (send-command
    session
    :get
    ((session-data-base session) "/element/" element-id "/enabled")))

;GET 	/session/:sessionId/element/:id/equals
(defun element-equals (session element-id)
  (send-command
    session
    :get
    ((session-data-base session) "/element/" element-id "/equals")))

;GET 	/session/:sessionId/element/:id/location
(defun get-element-location (session element-id)
  (send-command
    session
    :get
    ((session-data-base session) "/element/" element-id "/location")))

;GET 	/session/:sessionId/element/:id/location_in_view
(defun get-element-location-in-view (session element-id)
  (send-command
    session
    :get
    ((session-data-base session) "/element/" element-id "/location_in_view")))

;GET 	/session/:sessionId/element/:id/name
(defun get-element-name (session element-id)
  (send-command
    session
    :get
    ((session-data-base session) "/element/" element-id "/name")))

;GET 	/session/:sessionId/element/:id/screenshot
(defun take-element-screenshot (session element-id)
  (send-command
    session
    :get
    ((session-data-base session) "/element/" element-id "/screenshot")))

;GET 	/session/:sessionId/element/:id/selected
(defun is-element-selected (session element-id)
  (send-command
    session
    :get
    ((session-data-base session) "/element/" element-id "/selected")))

;GET 	/session/:sessionId/element/:id/size
(defun get-element-size (session element-id)
  (send-command
    session
    :get
    ((session-data-base session) "/element/" element-id "/size")))

;GET 	/session/:sessionId/element/:id/text
(defun get-element-text (session element-id)
  (send-command
    session
    :get
    ((session-data-base session) "/element/" element-id "/text")))

;POST 	/session/:sessionId/element/:id/value
(defun get-element-value (session element-id)
  (send-command
    session
    :post
    ((session-data-base session) "/element/" element-id "/value")))

