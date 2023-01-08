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

; Get Element Attribute
; HTTP Command: GET
; Path:         /session/:sessionId/element/:id/attribute/:name
; This function is not yet implemented.
(defun get-element-attribute (session element-id attribute-name)
  (send-command
    session
    :get
    ((session-data-base session) "/element/" element-id "/attribute/" attribute-name)))

; Is Element Displayed
; HTTP Command: GET
; Path:         /session/:sessionId/element/:id/displayed
; This function is not yet implemented.
(defun is-element-displayed (session element-id)
  (send-command
    session
    :get
    ((session-data-base session) "/element/" element-id "/displayed")))

; Is Element Enabled
; HTTP Command: GET
; Path:         /session/:sessionId/element/:id/enabled
; This function is not yet implemented.
(defun is-element-enabled (session element-id)
  (send-command
    session
    :get
    ((session-data-base session) "/element/" element-id "/enabled")))

; Element Equals
; HTTP Command: GET
; Path:         /session/:sessionId/element/:id/equals
; This function is not yet implemented.
(defun element-equals (session element-id1 element-id2)
  (send-command
    session
    :get
    ((session-data-base session) "/element/" element-id1 "/equals/" element-id2)))

; Get Element Location
; HTTP Command: GET
; Path:         /session/:sessionId/element/:id/location
; This function is not yet implemented.
(Defun get-element-location (Session element-id)
  (send-command
    session
    :get
    ((session-data-base session) "/element/" element-id "/location")))

; Get Element Location in View
; HTTP Command: GET
; Path:         /session/:sessionId/element/:id/location_in_view
; This function is not yet implemented.
(defun get-element-location-in-view (session element-id)
  (send-command
    session
    :get
    ((session-data-base session) "/element/" element-id "/location_in_view")))

; Get Element Name
; HTTP Command: GET
; Path:         /session/:sessionId/element/:id/name
; This function is not yet implemented.
(defun get-element-name (session element-id)
  (send-command
    session
    :get
    ((session-data-base session) "/element/" element-id "/name")))

; Is Element Selected
; HTTP Command: GET
; Path:         /session/:sessionId/element/:id/selected
; This function is not yet implemented.
(defun is-element-selected (session element-id)
  (send-command
    session
    :get
    ((session-data-base session) "/element/" element-id "/selected")))

; Get Element Size
; HTTP Command: GET
; Path:         /session/:sessionId/element/:id/size
; This function is not yet implemented.
(defun get-element-size (session element-id)
  (send-command
    session
    :get
    ((session-data-base session) "/element/" element-id "/size")))

; Get Element Text
; HTTP Command: GET
; Path:         /session/:sessionId/element/:id/text
(defun get-element-text (session element-id)
  (send-command
    session
    :get
    ((session-data-base session) "/element/" element-id "/text")))

