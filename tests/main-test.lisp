;;;; MIT License
;;;; 
;;;; Copyright (c) 2022-2023 kzO8Xekb
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

(subtest "Testing create-session."
         (let
           (session)
           (is-type
             (setf session (win-app-driver::create-session))
             'function)))

; ToDo: The following test code would fail. The reason is not known.
;(subtest "Testing with-session."
;         (is-expand
;           (win-app-driver:with-session (foo)
;             nil)
;           (let ()
;             (let ()
;               (let (($session (win-app-driver:create-session)))
;                 (unwind-protect
;                   (win-app-driver::flet*
;                     ((foo (&rest $session-command-args)
;                           (apply $session $session-command-args))
;                      ($progn-forms ()
;                                    nil))
;                     (declare (dynamic-extent (function $progn-forms)))
;                     (funcall $session
;                              :new-session
;                              :app "root"
;                              :app-arguments nil
;                              :app-top-level-window nil
;                              :app-working-dir nil
;                              :device-name "WindowsPC"
;                              :msex-experimental-webdriver nil
;                              :msex-wait-for-app-launch nil
;                              :host "localhost"
;                              :platform-name "Windows"
;                              :platform-version nil
;                              :port 4723)
;                     ($progn-forms))
;                   (funcall $session :delete-session))))))
;
;         (is-expand
;           (with-session (test :app "Root")
;             (declare (optimize (speed 3) (space 0) (safety 0) (debug 0)))
;             "This is Test."
;             (test :find-element :class-name "FOO")
;             (test :element-clear))
;           (let ()
;             (let ()
;               (declare (optimize (speed 3) (space 0) (safety 0) (debug 0)))
;               (let (($session (win-app-driver:create-session)))
;                 (unwind-protect
;                   (win-app-driver:flet*
;                     ((test (&rest $session-command-args)
;                            (apply $session $session-command-args))
;                      ($progn-forms ()
;                                    "this is test."
;                                    (test :find-element :class-name "foo")
;                                    (test :element-clear)))
;                     (declare (dynamic-extent (function $progn-forms)))
;                     (funcall $session
;                              :new-session
;                              :app "root"
;                              :app-arguments nil
;                              :app-top-level-window nil
;                              :app-working-dir nil
;                              :device-name "WindowsPC"
;                              :msex-experimental-webdriver nil
;                              :msex-wait-for-app-launch nil
;                              :host "localhost"
;                              :platform-name "Windows"
;                              :platform-version nil
;                              :port 4723)
;                     ($progn-forms))
;                   (funcall $session :delete-session)))))))

