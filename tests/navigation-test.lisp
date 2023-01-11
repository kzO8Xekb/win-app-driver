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

(in-package :win-app-driver/tests)

(let
  ((edge-session (win-app-driver::create-session)))

  (funcall                                                ; Run MS Edge
    edge-session
    :new-session
    :host *win-app-driver-host*
    :port *win-app-driver-port*
    :app "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe"
    :app-arguments "-private")

  (let
    ((base (win-app-driver::session-data-base
             (funcall
               edge-session
               :pandoric-get
               'win-app-driver::session))))

    (subtest "Testing back."
             (test-api
               (funcall edge-session :back)
               :content-length "63"
               :path           (concatenate
                                 'string
                                 base
                                 "/back")))

    (subtest "Testing forward."
             (test-api
               (funcall edge-session :forward)
               :content-length "63"
               :path           (concatenate
                                 'string
                                 base
                                 "/forward")))

    (subtest "Testing get-title."
             (test-api
               (funcall
                 edge-session
                 :get-title)
               :content-length (write-to-string
                                 (+ 90 (length (win-app-driver::get-value $json))))
               :path           (concatenate
                                 'string
                                 base
                                 "/title")
               :app            (ok
                                 (win-app-driver::get-value $json))
               :platform-name  (ok
                                 (win-app-driver::get-value $json))
               :value          (ok
                                 (win-app-driver::get-value $json))))

    (subtest "Testing take-element-screenshot."
             (let
               ((element-id (win-app-driver::get-element-id
                              (funcall
                                edge-session
                                :find-element
                                :class-name "BrowserRootView"))))
               (test-api
                 (funcall
                   edge-session
                   :take-element-screenshot
                   element-id)
                 :content-length (write-to-string
                                   (+ 74 (length (win-app-driver::get-value $json))))
                 :path           (concatenate
                                   'string
                                   base
                                   "/element/"
                                   element-id
                                   "/screenshot")
                 :app            (like
                                   (win-app-driver::get-value $json)
                                   *base64-regex*)
                 :platform-name  (like
                                   (win-app-driver::get-value $json)
                                   *base64-regex*)
                 :value          (like
                                   (win-app-driver::get-value $json)
                                   *base64-regex*))))

    (funcall edge-session :delete-session)))

