# win-app-driver

[Windows Application Driver (WinAppDriver)](https://github.com/microsoft/WinAppDriver) is a service to support Selenium-like UI Test Automation on Windows Applications. This package allows WinAppDriver to be used with Common Lisp.

This is a prototype. It has not been fully tested. Please note that not all functions may work properly.

In particular, functions that require the use of "Appium" should not work at all.

## Requirements

1. Windows 10 PC with the latest Windows 10 version (Version 1607 or later)
1. [WinAppDriver](https://github.com/microsoft/WinAppDriver).
1. [Steel Bank Common Lisp(SBCL) 2.2.4 for Windows](https://www.sbcl.org/)

### Common Lisp packages
1. cl-ppcre
1. dexador
1. jonathan
1. let-over-lambda
1. local-time
1. trivial-macroexpand-all

### Tools to assist with element id research.
1. [Accessibility Insights for Windows](https://accessibilityinsights.io/)
1. [WinAppDriver UI Recorder Tool](https://github.com/microsoft/WinAppDriver/tree/master/Tools/UIRecorder) Supplementary tools for WinAppDriver.

## Usage
1. Download and install WinAppDriver from the [official website](https://github.com/microsoft/WinAppDriver).
1. Start WinAppDriver Server.
1. Extract the complete set of win-app-driver files to your quicklisp/local-projects folder.
1. Start SBCL.
1. Perform as follows.
```common-lisp
(ql:quickload :win-app-driver)

(setf (symbol-function 'notepad-session) (win-app-driver::create-session))
;#<FUNCTION (LAMBDA (&REST #:ARGS539) :IN WIN-APP-DRIVER:CREATE-SESSION) {10025B50FB}>

(notepad-session
  :new-session
  :host "localhost"                       ; optional default "localhost".
  :port 12345                             ; optional default 4723.
  :app "C:/Windows/System32/notepad.exe") ; "\" does not work. Please change to "/".
;"{\"sessionId\":\"CF3B1FB7-E4F5-485F-AB3A-D8CDEAD48E56\",\"status\":0,\"value\":{\"app\":\"C:/Windows/System32/notepad.exe\",\"platformName\":\"Windows\"}}"
;200
;#<HASH-TABLE :TEST EQUAL :COUNT 4 {10025C42D3}>
;#<QURI.URI.HTTP:URI-HTTP http://localhost:12345/session>

(notepad-session
  :status)
;"{\"build\":{\"revision\":\"2003\",\"time\":\"Wed Aug 26 07:56:06 2020\",\"version\":\"1.2.2009\"},\"os\":{\"arch\":\"amd64\",\"name\":\"windows\",\"version\":\"10.0.22000\"}}"
;200
;#<HASH-TABLE :TEST EQUAL :COUNT 4 {10025C74B3}>
;#<QURI.URI.HTTP:URI-HTTP http://localhost:12345/status>

(notepad-session
  :find-element
  :name "Edit")
;"{\"sessionId\":\"CF3B1FB7-E4F5-485F-AB3A-D8CDEAD48E56\",\"status\":0,\"value\":{\"ELEMENT\":\"42.1905664.4.10\"}}"
;200
;#<HASH-TABLE :TEST EQUAL :COUNT 4 {10025CA4B3}>
;#<QURI.URI.HTTP:URI-HTTP http://localhost:12345/session/CF3B1FB7-E4F5-485F-AB3A-D8CDEAD48E56//element>

(notepad-session
  :find-element
  :name "Text editor")
;"{\"sessionId\":\"CF3B1FB7-E4F5-485F-AB3A-D8CDEAD48E56\",\"status\":0,\"value\":{\"ELEMENT\":\"42.3802786\"}}"
;200
;#<HASH-TABLE :TEST EQUAL :COUNT 4 {10025CD483}>
;#<QURI.URI.HTTP:URI-HTTP http://localhost:12345/session/CF3B1FB7-E4F5-485F-AB3A-D8CDEAD48E56//element>

(progn
  (notepad-session
    :element-click "42.3802786") ; The argument is the ELEMENT investigated in find-element.
  (notepad-session
    :send-string "hello, world"))
;"{\"sessionId\":\"CF3B1FB7-E4F5-485F-AB3A-D8CDEAD48E56\",\"status\":0}"
;200
;#<HASH-TABLE :TEST EQUAL :COUNT 4 {10025E52F3}>
;#<QURI.URI.HTTP:URI-HTTP http://localhost:12345/session/CF3B1FB7-E4F5-485F-AB3A-D8CDEAD48E56//keys>

(notepad-session
  :get-element-text "42.3802786")
;"{\"sessionId\":\"CF3B1FB7-E4F5-485F-AB3A-D8CDEAD48E56\",\"status\":0,\"value\":\"hello, world\"}"
;200
;#<HASH-TABLE :TEST EQUAL :COUNT 4 {10025E7C03}>
;#<QURI.URI.HTTP:URI-HTTP http://localhost:12345/session/CF3B1FB7-E4F5-485F-AB3A-D8CDEAD48E56//element/42.3802786/text>
```
## Installation

Extract the complete set of win-app-driver files to your quicklisp/local-projects folder.

## Tips

## Author

* kzO8Xekb

## Copyright

Copyright (c) 2022 kzO8Xekb

