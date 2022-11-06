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
;;;;

(in-package :win-app-driver)

;; Conditionの定義/使用関係のマクロ

(defmacro make-condition-if (pfunc args signal-type condition caller)
  `(when (funcall ,pfunc ,@args)
     (,signal-type
       (make-condition
         ,condition
         :caller ,caller))))

(defun make-error-condition-if (pfunc condition caller)
  "make-error-condition-if
   _/_/_/ 概要 _/_/_/
   引数で指定した述語関数の結果がnilでないとき，errorコンディションを発報します。

   _/_/_/ 引数 _/_/_/
   pfunc, (FUNCTION NIL (VALUES BOOLEAN &OPTIONAL)), 無引数の述語関数を指定します。
   condition, CONDITION, 発報するコンディションを指定します。
   caller, string, 呼び出し元の関数名の文字列を指定します。

   _/_/_/ 返り値 _/_/_/
   (VALUES NULL &OPTIONAL)"
  (make-condition-if pfunc nil error condition caller))

(defun make-warning-condition-if (pfunc condition caller)
  "make-warning-condition-if
  _/_/_/ 概要 _/_/_/
  引数で指定した述語関数の結果がnilでないとき，warningコンディションを発報します。

  _/_/_/ 引数 _/_/_/
  pfunc, (FUNCTION NIL (VALUES BOOLEAN &OPTIONAL)), 無引数の述語関数を指定します。
  condition, CONDITION, 発報するコンディションを指定します。
  caller, string, 呼び出し元の関数名の文字列を指定します。

  _/_/_/ 返り値 _/_/_/
  (VALUES NULL &OPTIONAL)"
  (make-condition-if pfunc nil warn condition caller))

(defun make-signal-condition-if (pfunc condition caller)
  "make-signal-condition-if
  _/_/_/ 概要 _/_/_/
  引数で指定した述語関数の結果がnilでないとき，signalコンディションを発報します。

  _/_/_/ 引数 _/_/_/
  pfunc, (FUNCTION NIL (VALUES BOOLEAN &OPTIONAL)), 無引数の述語関数を指定します。
  condition, CONDITION, 発報するコンディションを指定します。
  caller, symbol, 呼び出し元の関数のシンボルを指定します。

  _/_/_/ 返り値 _/_/_/
  (VALUES NULL &OPTIONAL)"
  (make-condition-if pfunc nil signal condition caller))

(lol:defmacro! progn-with-recovery-conditions (labels-args handler-args &rest body)
  ; アナフォリックマクロ progn-with-recovery-conditions
  ; _/_/_/ 概要 _/_/_/
  ; コンディションを発報した場合，リカバリ処理を行う。
  `(let
     (,g!result)
     (tagbody
       ,g!tag-start
       (flet
         ((try-again () (go ,g!tag-start))             ; やり直すための内部関数(アナフォラ)
          (finish () (go ,g!tag-finish)))              ; ハンドリングを抜け出すための内部関数(アナフォラ)
         (labels
           (,@labels-args)
           (handler-bind
             (,@handler-args)
             (setf ,g!result
                   (progn
                     ,@body))
             (finish))))
       ,g!tag-finish)
     ,g!result))

(lol:defmacro! progn-on-preconditions (caller-sym signaling-func labels-args handler-args &rest body)
  ; アナフォリックマクロ progn-on-preconditions
  ; _/_/_/ 概要 _/_/_/
  ; body部の実行を開始する前に、第1引数で指定した発報関数がコンディションを発報した場合，リカバリ処理を行う。
;  `(let
;     (,g!result)
;     (tagbody
;       ,g!tag-start
;       (flet
;         ((try-again () (go ,g!tag-start))            ; やり直すための内部関数(アナフォラ)
;          (finish () (go ,g!tag-finish)))             ; ハンドリングを抜け出すための内部関数(アナフォラ)
;         (labels
;           (,@labels-args)
;           (handler-bind
;             (,@handler-args)
;             (when (not (null ,signaling-func))
;               (funcall ,signaling-func ,caller-sym)) ; シグナルを発報する状況ならシグナルを発報する関数。
;             (setf ,g!result
;                   (progn
;                     ,@body))
;             (finish))))
;       ,g!tag-finish)
;     ,g!result))
  `(progn-with-recovery-conditions
     ,labels-args
     ,handler-args
     (progn
       (funcall ,signaling-func ,caller-sym)
       ,@body)))

(lol:defmacro! progn-to-postconditions (caller-sym signaling-func labels-args handler-args &rest body)
  ; アナフォリックマクロ progn-to-postconditions
  ; _/_/_/ 概要 _/_/_/
  ; body部の実行後に、第1引数で指定した発報関数がコンディションを発報した場合，リカバリ処理を行う。
;  `(let
;     (,g!result)
;     (tagbody
;       ,g!tag-start
;       (flet
;         ((try-again () (go ,g!tag-start))             ; やり直すための内部関数(アナフォラ)
;          (finish () (go ,g!tag-finish)))              ; ハンドリングを抜け出すための内部関数(アナフォラ)
;         (labels
;           (,@labels-args)
;           (handler-bind
;             (,@handler-args)
;             (setf ,g!result
;                   (progn
;                     ,@body))
;             (when (not (null ,signaling-func))
;               (funcall ,signaling-func ,caller-sym))  ; シグナルを発報する状況ならシグナルを発報する関数。
;             (finish))))
;       ,g!tag-finish)
;     ,g!result))
  (let
    (g!result)
    `(progn-with-recovery-conditions
       ,labels-args
       ,handler-args
       (progn
         (setf ,g!result ,@body)
         (funcall ,signaling-func ,caller-sym)
         ,g!result))))

(defmacro define-win-app-driver-condition (condition-name condition-type report-message)
  `(define-condition ,condition-name (,condition-type)
     ((log-time :initform (local-time:now)
                :reader log-time)
      (caller :initarg :caller
              :reader  caller)
      (message :initarg :message
               :reader  message))
     (:report (lambda (condition stream)
                (format
                  stream
                  ,report-message
                  (log-time condition)
                  (caller condition)
                  (message condition))))))

;(define-condition win-app-driver-error-condition (simple-error)
;  ((log-time :initform (local-time:now)
;             :reader log-time)
;   (caller :initarg :caller
;           :reader  caller)
;   (message :initarg :message
;            :reader  message))
;  (:report (lambda (condition stream)
;             (format
;               stream
;               "[~A][ERROR]@~A: ~A~%"
;               (log-time condition)
;               (caller condition)
;               (message condition)))))

;; このプロジェクトで使うコンディションのルート
(define-win-app-driver-condition win-app-driver-error-condition   simple-error     "[~A][ERROR]@~A: ~A~%")
(define-win-app-driver-condition win-app-driver-warning-condition simple-warning   "[~A][WARNING]@~A: ~A~%")
(define-win-app-driver-condition win-app-driver-simple-condition  simple-condition "[~A][INFO]@~A: ~A~%")

(eval-when (:compile-toplevel)
  (defun expand-define-condition (args)
    `(define-condition ,(nth 0 args) (,(nth 1 args))
       ((log-time :initform (local-time:now)                                            ; signal発報日時
                  :reader log-time)
        (caller   :initarg :caller                                                      ; 呼び出し元関数又はマクロ関数
                  :reader  caller)
        (message  :initform ,(nth 2 args)                                               ; ユーザーに示すメッセージ内容
                  :reader message)
        (signal-type :initform ,(cond                                                   ; signalの種類に応じたコード生成する
                                  ((eq (second args) 'win-app-driver-error-condition)   ; エラーコンディションの場合
                                   "ERROR")
                                  ((eq (second args) 'win-app-driver-warning-condition) ; 警告コンディションの場合
                                   "WARNING")
                                  ((eq (second args) 'win-app-driver-simple-condition)  ; 一般コンディションの場合
                                   "INFOMATION"))
                     :reader signal-type))
       (:report (lambda (condition stream)
                  (format stream
                          "[~A][~A]@~A: ~A~%"
                          (log-time    condition)
                          (signal-type condition)
                          (caller      condition)
                          (message     condition)))))))

(defmacro define-conditions (&rest define-list)
  `(progn
     ,@(mapcar #'expand-define-condition define-list)))

(define-conditions
  (condition-incorrect-arguments        win-app-driver-error-condition "Incorrect arguments have been given to the function or macro function. Correct them with the appropriate arguments. If the error is caused by a function, (describe 'error-function-symbol) may provide useful information.")
  (condition-incorrect-hostname-string  win-app-driver-error-condition "Incorrect hostname string. The given string does not meet the hostname specification. The hostname must be a string that conforms to RFC 952 and RFC 1123.")
  (condition-incorrect-port-number      win-app-driver-error-condition "Incorrect port number. The port number must be a natural number between 1 and 65535.")
  (condition-unexpected-error           win-app-driver-error-condition "Unexpected error.")
  (condition-unknown-http-method-error  win-app-driver-error-condition "Unknown http method error.")
  )

