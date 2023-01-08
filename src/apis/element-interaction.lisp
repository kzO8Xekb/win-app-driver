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

(defparameter *keymap*
  (alexandria:plist-hash-table
    (list
      :null        "\"\\ue000\""
      :cancel      "\"\\ue001\""
      :help        "\"\\ue002\""
      :backspace   "\"\\ue003\""
      :tab         "\"\\ue004\""
      :clear       "\"\\ue005\""
      :return      "\"\\ue006\""
      :enter       "\"\\ue007\""
      :shift       "\"\\ue008\""
      :control     "\"\\ue009\""
      :ctrl        "\"\\ue009\""
      :alt         "\"\\ue00a\""
      :pause       "\"\\ue00b\""
      :escape      "\"\\ue00c\""
      :space       "\"\\ue00d\""
      :page-up     "\"\\ue00e\""
      :page-down   "\"\\ue00f\""
      :end         "\"\\ue010\""
      :home        "\"\\ue011\""
      :left-arrow  "\"\\ue012\""
      :up-arrow    "\"\\ue013\""
      :right-arrow "\"\\ue014\""
      :down-arrow  "\"\\ue015\""
      :insert      "\"\\ue016\""
      :delete      "\"\\ue017\""
      :semicolon   "\"\\ue018\""
      :equals      "\"\\ue019\""
      :numpad-0    "\"\\ue01a\""
      :numpad-1    "\"\\ue01b\""
      :numpad-2    "\"\\ue01c\""
      :numpad-3    "\"\\ue01d\""
      :numpad-4    "\"\\ue01e\""
      :numpad-5    "\"\\ue01f\""
      :numpad-6    "\"\\ue020\""
      :numpad-7    "\"\\ue021\""
      :numpad-8    "\"\\ue022\""
      :numpad-9    "\"\\ue023\""
      :multiply    "\"\\ue024\""
      :add         "\"\\ue025\""
      :separator   "\"\\ue026\""
      :substract   "\"\\ue027\""
      :decimal     "\"\\ue028\""
      :divide      "\"\\ue029\""
      :f1          "\"\\ue031\""
      :f2          "\"\\ue032\""
      :f3          "\"\\ue033\""
      :f4          "\"\\ue034\""
      :f5          "\"\\ue035\""
      :f6          "\"\\ue036\""
      :f7          "\"\\ue037\""
      :f8          "\"\\ue038\""
      :f9          "\"\\ue039\""
      :f10         "\"\\ue03a\""
      :f11         "\"\\ue03b\""
      :f12         "\"\\ue03c\""
      :meta        "\"\\ue03d\""))
  "See https://www.w3.org/TR/webdriver1/#keyboard-actions")

(defun converte-key-list-to-json-value-string (keys)
  "(converte-key-list-to-json-value-string '(#\t #\e #\s #\t :enter)) => \"[\"t\",\"e\",\"s\",\"t\",\"\\ue007\"]\""
  (flet
    ((character-to-key (char)
                       (multiple-value-bind
                         (key success)
                         (gethash char *keymap*)
                         (if success
                           key
                           (concatenate 'string "\"" (string char) "\"")))))
    (let
      ((first-step t)
       (result "["))
      (loop :for itr :in keys
            :do (cond
                  (first-step
                    (setf result (concatenate 'string result (character-to-key itr)))
                    (setq first-step nil))
                  (t
                    (setf result (concatenate 'string result "," (character-to-key itr))))))
      (concatenate 'string result "]"))))

; Send Keys
; HTTP Command: POST
; Path:         /session/:sessionId/keys
(defun send-keys (session keys)
  (let
    ((val (concatenate
            'string
            "{\"value\":"
            (converte-key-list-to-json-value-string keys)
            "}")))
    (send-command
      session
      :post
      ((session-data-base session) "/keys")
      val)))

; Send String
(defun send-string (session &rest strings)
  (flet
    ((convert-keys (itr)
                   (if (stringp itr)
                     (concatenate 'list itr)
                     (list itr))))
    (send-keys
      session
      (alexandria:flatten
        (mapcar
          #'convert-keys
          strings)))))

; Element Send Keys
; HTTP Command: POST
; Path:         /session/:sessionId/element/:id/value
; see https://www.w3.org/TR/webdriver/#element-send-keys
(defun element-send-keys (session element-id keys)
  (let
    ((val (concatenate
            'string
            "{\"value\":"
            (converte-key-list-to-json-value-string keys)
            "}")))
    (send-command
      session
      :post
      ((session-data-base session) "/element/" element-id "/value")
      val)))

; Element Send String
(defun element-send-string (session element-id &rest string)
  (flet
    ((convert-keys (itr)
                   (if (stringp itr)
                     (concatenate 'list itr)
                     (list itr))))
    (element-send-keys
      session
      element-id
      (alexandria:flatten
        (mapcar
          #'convert-keys
          string)))))

; Element Clear
; HTTP Command: POST
; Path:         /session/:sessionId/element/:id/clear
; This function is not yet implemented.
(defun element-clear (session element-id)
  (send-command
    session
    :post
    ((session-data-base session) "/element/" element-id "/clear")))

; Element Click
; HTTP Command: POST
; Path:         /session/:sessionId/element/:id/click
(defun element-click (session element-id)
  (send-command
    session
    :post
    ((session-data-base session) "/element/" element-id "/click")))

