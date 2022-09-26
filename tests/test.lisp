(defpackage edge-webdriver/tests
  (:use :cl
        :edge-webdriver
        :prove)
  (:export maptree/df-testing-subfn-if-even-collect))

(in-package :edge-webdriver/tests)

(defmacro mac (expr)
  `(pprint (macroexpand-1 ',expr)))

(defmacro mac-all (expr)
  `(pprint (trivial-macroexpand-all:macroexpand-all ',expr)))

;; NOTE: To run this test file, execute `(asdf:test-system :edge-webdriver)' in your Lisp.

(setf prove:*enable-colors* nil)

(plan nil)

(defparameter *edge-webdriver-host* "localhost")
(defparameter *edge-webdriver-port* 35367)

;; Test for macro with-session
; (defun open-foo-site (url)
;   (with-session
;     (wds :port 35367 :host "localhost")
;     (wds :maximize-window)
;     (wds :navigate-to url)
;     (foo (wds :get-element-text (wds :get-session-id :xpath "//bar")))))
; =>
; (defun open-foo-site (url)
;   (let
;     ((,(first args) (create-session))
;      ($host (aif (getf (rest args) :host) it "localhost"))
;      ($port (aif (getf (rest args) :port) it 9515)))
;     (unwind-protect
;       (progn
;         (funcall ,(first args) :new-session :host $host :port $port)
;         (progn
;           (funcall ,(first args) :maximize-window)                                                                 ; bodyを深さ優先で展開させる
;           (funcall ,(first args) :navigate-to url)                                                                 ;
;           (foo (funcall ,(first args) :get-element-text (funcall ,(first args) :get-session-id :xpath "//bar"))))) ;
;       (progn
;         (funcall ,(first args) :delete-session)))))
(defun maptree/df-testing-subfn-if-even-collect (left right)
  "testing sample closure"
  (cond
    ((and
       (integerp left)
       (integerp right)
       (evenp left)
       (evenp right))
     (cons left `(,right)))
    ((and
       (integerp left)
       (oddp left)
       (consp right))
     right)
    ((and
       (integerp right)
       (oddp right)
       (consp left))
     left)
    ((and
       (integerp left)
       (oddp left)
       (null right))
     nil)
    ((and
       (integerp left)
       (oddp left))
     `(,right))
    ((and
       (null left)
       (integerp right)
       (oddp right))
     nil)
    ((and
       (integerp right)
       (oddp right))
     (cons left nil))
    ((and
       (null left)
       (consp right))
     right)
    ((and
       (null left)
       (null right))
     nil)
    (t
      (cons left right))))

(subtest "Test for function maptree/df"
         (is
           (edge-webdriver::maptree/df (lambda (x) x) nil)
           nil)

         (is
           (edge-webdriver::maptree/df (lambda (x) (* 2 x)) nil)
           nil)

         (is
           (edge-webdriver::maptree/df (lambda (x) x) (list 1))
           (list 1))

         (is
           (edge-webdriver::maptree/df (lambda (x) x) `(1 2 (3 4 (5) (6 (7)) 8)))
           `(1 2 (3 4 (5) (6 (7)) 8)))

         (is
           (edge-webdriver::maptree/df (lambda (x) (+ x x)) `(1 2 (3 4 (5) (6 (7)) 8)))
           `(2 4 (6 8 (10) (12 (14)) 16)))

         (is
           (edge-webdriver::maptree/df
             (lambda (x) x)
             nil
             :collector #'edge-webdriver/tests:maptree/df-testing-subfn-if-even-collect)
           nil)

         (is
           (edge-webdriver::maptree/df
             (lambda (x) x)
             `(1)
             :collector #'edge-webdriver/tests:maptree/df-testing-subfn-if-even-collect)
           nil)

         (is
           (edge-webdriver::maptree/df
             (lambda (x) x)
             `(2)
             :collector #'edge-webdriver/tests:maptree/df-testing-subfn-if-even-collect)
           `(2))

         (is
           (edge-webdriver::maptree/df
             (lambda (x) x)
             `(1 2 (3 4 (5) (6 (7)) 8))
             :collector #'edge-webdriver/tests:maptree/df-testing-subfn-if-even-collect)
           `(2 (4 (6) 8)))

         (is
           (edge-webdriver::maptree/df
             (lambda (x) (* 2 x))
             `(1 2 (3 4 (5) (6 (7)) 8))
             :collector #'edge-webdriver/tests:maptree/df-testing-subfn-if-even-collect)
           `(2 4 (6 8 (10) (12 (14)) 16)))
         )

(subtest "Testing generate-desired-capabilities"
         (is
           (edge-webdriver::generate-desired-capabilities nil)
           "{\"desiredCapabilities\":{}}")

         (is
           (edge-webdriver::generate-desired-capabilities
             '(:|args| ("start-maximized" "user-data-dir=/tmp/temp_profile")))
           "{\"desiredCapabilities\":{\"ms:edgeOptions\":{\"args\":[\"start-maximized\",\"user-data-dir=/tmp/temp_profile\"]}}}")

         (is
           (edge-webdriver::generate-desired-capabilities
             '(:|args| ("start-maximized" "user-data-dir=/tmp/temp_profile")
               :|binary| "/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge"))
           "{\"desiredCapabilities\":{\"ms:edgeOptions\":{\"args\":[\"start-maximized\",\"user-data-dir=/tmp/temp_profile\"],\"binary\":\"/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge\"}}}")
         )

(subtest "Test for macro expand-with-session-body"
         (is-expand
           (expand-with-session-body
             wds
             (wds
               :maximize-window)
             (wds
               :navigate-to
               url)
             (foo
               (wds
                 :get-element-text
                 (wds
                   :get-session-id
                   :xpath
                   "//bar"))))
           (progn
             (funcall
               wds
               :maximize-window)
             (funcall
               wds
               :navigate-to
               url)
             (foo
               (funcall
                 wds
                 :get-element-text
                 (funcall
                   wds
                   :get-session-id
                   :xpath
                   "//bar"))))))

(subtest "test for macro with-session"
         (is-expand
           (with-session
             (wds)
             nil)
           (let
             ((wds (edge-webdriver::create-session))
              ($0 (let-over-lambda:aif
                    nil let-over-lambda:it
                    "localhost"))
              ($1 (let-over-lambda:aif
                    nil let-over-lambda:it
                    9515)))
             (unwind-protect
               (progn
                 (funcall wds
                          :new-session
                          :host $0
                          :port $1)
                 (edge-webdriver::expand-with-session-body
                   wds
                   nil))
               (progn
                 (funcall wds
                          :delete-session)))))

         (is-expand
           (with-session
             (wds)
             (wds :maximize-window))
           (let
             ((wds (edge-webdriver::create-session))
              ($0 (let-over-lambda:aif
                    nil let-over-lambda:it
                    "localhost"))
              ($1 (let-over-lambda:aif
                    nil let-over-lambda:it
                    9515)))
             (unwind-protect
               (progn
                 (funcall wds
                          :new-session
                          :host $0
                          :port $1)
                 (edge-webdriver::expand-with-session-body
                   wds (wds :maximize-window)))
               (progn
                 (funcall wds
                          :delete-session)))))

         (is-expand
           (with-session
             (wds)
             (wds :maximize-window)
             (wds :navigate-to url)
             (foo (wds :get-element-text (wds :get-session-id :xpath "//bar"))))
           (let
             ((wds (edge-webdriver::create-session))
              ($0 (let-over-lambda:aif
                    nil let-over-lambda:it
                    "localhost"))
              ($1 (let-over-lambda:aif
                    nil let-over-lambda:it
                    9515)))
             (unwind-protect
               (progn
                 (funcall wds
                          :new-session
                          :host $0
                          :port $1)
                 (edge-webdriver::expand-with-session-body
                   wds
                   (wds :maximize-window)
                   (wds :navigate-to url)
                   (foo (wds :get-element-text (wds :get-session-id :xpath "//bar")))))
               (progn
                 (funcall wds
                          :delete-session)))))

         (is-expand
           (with-session
             (wds :port 12345 :host "127.0.0.1")
             (wds :maximize-window)
             (wds :navigate-to url)
             (foo (wds :get-element-text (wds :get-session-id :xpath "//bar"))))
           (let
             ((wds (edge-webdriver::create-session))
              ($0 (let-over-lambda:aif
                    "127.0.0.1" let-over-lambda:it
                    "localhost"))
              ($1 (let-over-lambda:aif
                    12345 let-over-lambda:it
                    9515)))
             (unwind-protect
               (progn
                 (funcall wds
                          :new-session
                          :host $0
                          :port $1)
                 (edge-webdriver::expand-with-session-body
                   wds
                   (wds :maximize-window)
                   (wds :navigate-to url)
                   (foo (wds :get-element-text (wds :get-session-id :xpath "//bar")))))
               (progn
                 (funcall wds
                          :delete-session)))))

         (is-expand
           (with-session
             (wds :host "127.0.0.1")
             (wds :maximize-window)
             (wds :navigate-to url)
             (foo (wds :get-element-text (wds :get-session-id :xpath "//bar"))))
           (let
             ((wds (edge-webdriver::create-session))
              ($0 (let-over-lambda:aif
                    "127.0.0.1" let-over-lambda:it
                    "localhost"))
              ($1 (let-over-lambda:aif
                    nil let-over-lambda:it
                    9515)))
             (unwind-protect
               (progn
                 (funcall wds
                          :new-session
                          :host $0
                          :port $1)
                 (edge-webdriver::expand-with-session-body
                   wds
                   (wds :maximize-window)
                   (wds :navigate-to url)
                   (foo (wds :get-element-text (wds :get-session-id :xpath "//bar")))))
               (progn
                 (funcall wds
                          :delete-session)))))

         (is-expand
           (with-session
             (wds :port 12345)
             (wds :maximize-window)
             (wds :navigate-to url)
             (foo (wds :get-element-text (wds :get-session-id :xpath "//bar"))))
           (let
             ((wds (edge-webdriver::create-session))
              ($0 (let-over-lambda:aif
                    nil let-over-lambda:it
                    "localhost"))
              ($1 (let-over-lambda:aif
                    12345 let-over-lambda:it
                    9515)))
             (unwind-protect
               (progn
                 (funcall wds
                          :new-session
                          :host $0
                          :port $1)
                 (edge-webdriver::expand-with-session-body
                   wds
                   (wds :maximize-window)
                   (wds :navigate-to url)
                   (foo (wds :get-element-text (wds :get-session-id :xpath "//bar")))))
               (progn
                 (funcall wds
                          :delete-session)))))

         (is-expand
           (with-session
             (wds :host "127.0.0.1" :port 12345)
             (wds :maximize-window)
             (wds :navigate-to url)
             (foo (wds :get-element-text (wds :get-session-id :xpath "//bar"))))
           (let
             ((wds (edge-webdriver::create-session))
              ($0 (let-over-lambda:aif
                    "127.0.0.1" let-over-lambda:it
                    "localhost"))
              ($1 (let-over-lambda:aif
                    12345 let-over-lambda:it
                    9515)))
             (unwind-protect
               (progn
                 (funcall wds
                          :new-session
                          :host $0
                          :port $1)
                 (edge-webdriver::expand-with-session-body
                   wds
                   (wds :maximize-window)
                   (wds :navigate-to url)
                   (foo (wds :get-element-text (wds :get-session-id :xpath "//bar")))))
               (progn
                 (funcall wds
                          :delete-session)))))

         (is-expand
           (with-session
             (wds :host "127.0.0.1"
                  :port 12345
                  :options '(:|args| ("start-maximized" "user-data-dir=/tmp/temp_profile")
                             :|binary| "/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge"))
             (wds :maximize-window)
             (wds :navigate-to url)
             (foo (wds :get-element-text (wds :get-session-id :xpath "//bar"))))
           (let
             ((wds (edge-webdriver::create-session))
              ($0 (let-over-lambda:aif
                    "127.0.0.1" let-over-lambda:it
                    "localhost"))
              ($1 (let-over-lambda:aif
                    12345 let-over-lambda:it
                    9515))
              ($2 (let-over-lambda:aif
                    '(:|args|
                       ("start-maximized"
                        "user-data-dir=/tmp/temp_profile")
                       :|binary|
                       "/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge")
                    let-over-lambda:it
                    nil)))
             (unwind-protect
               (progn
                 (funcall wds
                          :new-session
                          :host $0
                          :port $1
                          :options $2)
                 (edge-webdriver::expand-with-session-body
                   wds
                   (wds :maximize-window)
                   (wds :navigate-to url)
                   (foo (wds :get-element-text (wds :get-session-id :xpath "//bar")))))
               (progn
                 (funcall wds
                          :delete-session)))))

         )

;(with-session
;  (wds :host *edge-webdriver-host* :port *edge-webdriver-port*)
;  (subtest "Testing get-current-url and navigate-to"
;           (is
;             (wds :get-current-url)
;             "data:,")
;
;           (wds :navigate-to "https://hotel.testplanisphere.dev/ja/")
;
;           (is
;             (wds :get-current-url)
;             "https://hotel.testplanisphere.dev/ja/")))

(finalize)

