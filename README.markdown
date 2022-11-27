# win-app-driver

## Usage
``` Common Lisp
(ql:quickload :win-app-driver)

(setf (symbol-function 'notepad-session) (win-app-driver::create-session))
;#<FUNCTION (LAMBDA (&REST #:ARGS539) :IN WIN-APP-DRIVER:CREATE-SESSION) {10025B50FB}>

(notepad-session :new-session :host "localhost" :port 12345 :app "C:/Windows/System32/notepad.exe")
;"{\"sessionId\":\"CF3B1FB7-E4F5-485F-AB3A-D8CDEAD48E56\",\"status\":0,\"value\":{\"app\":\"C:/Windows/System32/notepad.exe\",\"platformName\":\"Windows\"}}"
;200
;#<HASH-TABLE :TEST EQUAL :COUNT 4 {10025C42D3}>
;#<QURI.URI.HTTP:URI-HTTP http://localhost:12345/session>

(notepad-session :status)
;"{\"build\":{\"revision\":\"2003\",\"time\":\"Wed Aug 26 07:56:06 2020\",\"version\":\"1.2.2009\"},\"os\":{\"arch\":\"amd64\",\"name\":\"windows\",\"version\":\"10.0.22000\"}}"
;200
;#<HASH-TABLE :TEST EQUAL :COUNT 4 {10025C74B3}>
;#<QURI.URI.HTTP:URI-HTTP http://localhost:12345/status>

(notepad-session :find-element :name "Edit")
;"{\"sessionId\":\"CF3B1FB7-E4F5-485F-AB3A-D8CDEAD48E56\",\"status\":0,\"value\":{\"ELEMENT\":\"42.1905664.4.10\"}}"
;200
;#<HASH-TABLE :TEST EQUAL :COUNT 4 {10025CA4B3}>
;#<QURI.URI.HTTP:URI-HTTP http://localhost:12345/session/CF3B1FB7-E4F5-485F-AB3A-D8CDEAD48E56//element>

(notepad-session :find-element :name "Text editor")
;"{\"sessionId\":\"CF3B1FB7-E4F5-485F-AB3A-D8CDEAD48E56\",\"status\":0,\"value\":{\"ELEMENT\":\"42.3802786\"}}"
;200
;#<HASH-TABLE :TEST EQUAL :COUNT 4 {10025CD483}>
;#<QURI.URI.HTTP:URI-HTTP http://localhost:12345/session/CF3B1FB7-E4F5-485F-AB3A-D8CDEAD48E56//element>

(progn
  (notepad-session :element-click "42.3802786")
  (notepad-session :send-string "hello, world"))
;"{\"sessionId\":\"CF3B1FB7-E4F5-485F-AB3A-D8CDEAD48E56\",\"status\":0}"
;200
;#<HASH-TABLE :TEST EQUAL :COUNT 4 {10025E52F3}>
;#<QURI.URI.HTTP:URI-HTTP http://localhost:12345/session/CF3B1FB7-E4F5-485F-AB3A-D8CDEAD48E56//keys>

(notepad-session :get-element-text "42.3802786")
;"{\"sessionId\":\"CF3B1FB7-E4F5-485F-AB3A-D8CDEAD48E56\",\"status\":0,\"value\":\"hello, world\"}"
;200
;#<HASH-TABLE :TEST EQUAL :COUNT 4 {10025E7C03}>
;#<QURI.URI.HTTP:URI-HTTP http://localhost:12345/session/CF3B1FB7-E4F5-485F-AB3A-D8CDEAD48E56//element/42.3802786/text>
```
## Installation

## Author

* kzO8Xekb

## Copyright

Copyright (c) 2022 kzO8Xekb
