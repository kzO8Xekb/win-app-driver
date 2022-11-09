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

(defparameter +http-header-accept+       "application/json, image/png")
(defparameter +http-header-content-type+ "application/json; charset=UTF-8")

(defstruct session-data
  (capabilities nil)
  (host          "")
  (port           0)
  (id            "")
  (base          ""))

;;; HTTP/HTTPS Utilities
(lol:defmacro! protect-for-timeout (&rest body)
 `(let
    (,g!dexador-response-hash-table
     ,g!dexador-response-quri-uri
     ,g!server-response-message
     ,g!server-response-status-code
     (,g!sleep-time -1))
    (tagbody
      ,g!tag-start                                               ; 処理巻き戻し時の開始点
      (handler-bind
        ((error                                                  ; errorコンディションを取得した場合
           (lambda (condition)
             (when (= 12002 (winhttp::win-error-code condition)) ; timeoutした場合
               (progn
                 (incf ,g!sleep-time)                            ; 待機時間を延ばす
                 (sleep ,g!sleep-time)                           ; 待機
                 (go ,g!tag-start))))))                          ; 巻き戻し!
        (progn
          (setf
            (values
              ,g!server-response-message
              ,g!server-response-status-code
              ,g!dexador-response-hash-table
              ,g!dexador-response-quri-uri)
            (progn ,@body))                                      ; 処理本体実行部
          (go ,g!tag-finish)))                                   ; 処理終了
      ,g!tag-finish)
    (values                                                      ; dexadorの返り値を多値で透過的に返す
      ,g!server-response-message
      ,g!server-response-status-code
      ,g!dexador-response-hash-table
      ,g!dexador-response-quri-uri)))

;(declaim (inline post-json))
(defun post-json (uri content)
  "uriのエンドポイントにjsonをPOSTします。"
  (dex:post uri                                                  ; uriにpostする
            :content content                                     ; postする文字列
            :headers `(("accept"       . ,+http-header-accept+)  ; headerの設定
                       ("accept-type"  . ,+http-header-accept+)
                       ("content-type" . ,+http-header-content-type+))))

;(declaim (inline post-json2))
(defun post-json2 (uri content)
  "uriのエンドポイントにjsonをPOSTします。"
  (post-json uri (jonathan:to-json content)))

;;-------------------------------------------------------------
;; macro-function invoke-win-app-driver-api
;; _/_/_/ 概要 _/_/_/
;; 引数で指定したWinAppDriverサーバのエンドポイントに，
;; HTTPクライアントを使ってリクエストするプログラムを生成する
;; マクロ関数。
;;
;; _/_/_/ 引数 _/_/_/
;; func,     function:     HTTPクライアントの関数を指定します。(例) #'dexador:get
;; endpoint, string:       WinAppServerのエンドポイントを文字列で指定します。
;; content,  [key]content: POSTする内容を指定します。
;;-------------------------------------------------------------
(lol:defmacro! invoke-win-app-driver-api (func endpoint &key (content nil))
 `(multiple-value-bind
    (,g!server-response-message                                ; WinAppDriverサーバーからのレスポンス。JSONで返ってくる。
     ,g!server-response-status-code                            ; HTTPレスポンス
     ,g!dexador-response-hash-table                            ; 不明。何かのハッシュテーブル。
     ,g!dexador-response-quri-uri)                             ; QURI.URI.HTTP:URI-HTTP, クエリを投げたURI
    (protect-for-timeout                                       ; タイムアウトを防ぐためにコンディションシステムで包むマクロ
      ,(if (null content)                                      ; contentはPOST以外はnilなので分岐する。
         `(funcall ,func ,endpoint)                            ; GET or DELETEの場合
         `(funcall ,func ,endpoint ,content)))                 ; POSTの場合
    (values                                                    ; HTTPクライアントの返り値を透過的に返す。
      ,g!server-response-message
      ,g!server-response-status-code
      ,g!dexador-response-hash-table
      ,g!dexador-response-quri-uri)))

(defun ad-get (endpoint) ; adはapplication driverの省略。
  (invoke-win-app-driver-api #'dexador:get endpoint))

(defun ad-delete (endpoint)
  (invoke-win-app-driver-api #'dexador:delete endpoint))

(defun ad-post (endpoint content)
  (invoke-win-app-driver-api
    #'post-json
    endpoint
    :content content))

(defun ipv4-address-p (host-identifier)
  "引数で指定した文字列がIPv4アドレスの文字列表現であるか照合します。"
  (multiple-value-bind
    (start end)
    (cl-ppcre:scan
      "^(([1-9]?[0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\.){3}([1-9]?[0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])$"
      host-identifier)
    (values start end)))

(defun rfc4291-address-p (host-identifier)
  "引数で指定した文字列がRFC4291に適合するIPv6アドレスの文字列表現であるか照合します。"
  (multiple-value-bind
    (start end)
    (cl-ppcre:scan
      "^((([0-9A-Fa-f]{1,4}:){7}([0-9A-Fa-f]{1,4}|:))|(([0-9A-Fa-f]{1,4}:){6}(:[0-9A-Fa-f]{1,4}|((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3})|:))|(([0-9A-Fa-f]{1,4}:){5}(((:[0-9A-Fa-f]{1,4}){1,2})|:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3})|:))|(([0-9A-Fa-f]{1,4}:){4}(((:[0-9A-Fa-f]{1,4}){1,3})|((:[0-9A-Fa-f]{1,4})?:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9A-Fa-f]{1,4}:){3}(((:[0-9A-Fa-f]{1,4}){1,4})|((:[0-9A-Fa-f]{1,4}){0,2}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9A-Fa-f]{1,4}:){2}(((:[0-9A-Fa-f]{1,4}){1,5})|((:[0-9A-Fa-f]{1,4}){0,3}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9A-Fa-f]{1,4}:){1}(((:[0-9A-Fa-f]{1,4}){1,6})|((:[0-9A-Fa-f]{1,4}){0,4}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(:(((:[0-9A-Fa-f]{1,4}){1,7})|((:[0-9A-Fa-f]{1,4}){0,5}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:)))(%.+)?$"
      host-identifier)
    (values start end)))

(defun ipv6-address-p (host-identifier)
  "引数で指定した文字列が\"[\"と\"]\"で囲まれたIPv6アドレスの文字列表現であるか照合します。"
  (multiple-value-bind
    (start end)
    (cl-ppcre:scan
      "^\\[((([0-9A-Fa-f]{1,4}:){7}([0-9A-Fa-f]{1,4}|:))|(([0-9A-Fa-f]{1,4}:){6}(:[0-9A-Fa-f]{1,4}|((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3})|:))|(([0-9A-Fa-f]{1,4}:){5}(((:[0-9A-Fa-f]{1,4}){1,2})|:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3})|:))|(([0-9A-Fa-f]{1,4}:){4}(((:[0-9A-Fa-f]{1,4}){1,3})|((:[0-9A-Fa-f]{1,4})?:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9A-Fa-f]{1,4}:){3}(((:[0-9A-Fa-f]{1,4}){1,4})|((:[0-9A-Fa-f]{1,4}){0,2}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9A-Fa-f]{1,4}:){2}(((:[0-9A-Fa-f]{1,4}){1,5})|((:[0-9A-Fa-f]{1,4}){0,3}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9A-Fa-f]{1,4}:){1}(((:[0-9A-Fa-f]{1,4}){1,6})|((:[0-9A-Fa-f]{1,4}){0,4}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(:(((:[0-9A-Fa-f]{1,4}){1,7})|((:[0-9A-Fa-f]{1,4}){0,5}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:)))(%.+)?\\]$"
      host-identifier)
    (values start end)))

(defun hostname-p (host-identifier)
  "引数で指定した文字列がRFC952及びRFC1123に適合する文字列か照合します。"
  (flet
    ((label-length-p (itr)                                     ; 文字列長に関する仕様を満たしているか判定する内部関数。
                     (and
                       (<= 1 (length host-identifier) 255)     ; ホスト名の文字列長は255文字以下である。
                       (<= 1 (length itr) 63)))                ; ホスト名のピリオドで区切られた各ラベルの文字列長は63文字以下である。
     (label-character-p (itr)                                  ; 各ラベルで使用可能な文字のみを使っているか判定する内部関数。
                        (multiple-value-bind
                          (start end)
                          (ppcre:scan "^(?!.*-$)[0-9A-Za-z\\-]+$" itr)
                          (values start end))))
    (let
      ((start 0)
       (end   0))
      (loop :for itr :in (ppcre:split "\\." host-identifier)   ; "."で分割し，各ラベルを検査する。
            :do (progn
                  (cond
                    ((and                                      ; 各ラベルが仕様を満たすか検査する。
                       (label-length-p itr)                    ; 文字列長の検査する。
                       (label-character-p itr))                ; 使用している文字の検査っする。
                     (incf end (length itr)))                  ; マッチしている文字列終了位置を延長する。
                    (t (setf start nil                         ; 仕様を満たさない場合
                             end   nil)
                       (return)))))                            ; ループを打ち切る。
      (values start end))))                                    ; 多値で返す。

(defun correct-hostname-string-p (host-identifier)
"correct-hostname-string-p
_/_/_/ 概要 _/_/_/
引数で指定した文字列が，ホスト名に適合するか照合します。
文字列がマッチした場合は，第1返り値を引数で指定した文字列，第2返り値をtとした多値で返します。
マッチしなければ，第1及び第2返り値ともにnilとした多値で返します。

_/_/_/ 引数 _/_/_/
host-identifier, string: 照合したい文字列を指定します。

_/_/_/ 返り値 _/_/_/
string or nil, 照合成功した場合は引数で指定した文字列。失敗時はnil。
t or nil,      関数の成功時はt，失敗時はnil。"
  (let
    (start end)
    (cond
      ((string= host-identifier "") (values nil nil))
      ((setf (values start end) (ipv4-address-p host-identifier))
       (values host-identifier t))
      ((setf (values start end) (ipv6-address-p host-identifier)) ; 正確にはRFC4291ではない。
       (values host-identifier t))
      ((setf (values start end) (hostname-p host-identifier))
       (values host-identifier t))
      (t (values nil t)))))

(defun correct-port-number-p (number)
  "引数で指定した整数がポート番号の範囲内の整数であるか検査します。"
  (declare (type (integer number)))
  (if (<= 1 number 65535)
    number
    nil))

(defun get-win-app-driver-host-uri (session)
  "引数で指定したsessionからホストURIの文字列を生成します。"
  (make-condition-if                                           ; hostnameを照合し，エラーがあればコンディションを発報する。
    (complement #'correct-hostname-string-p)
    ((session-data-host session))
    error
    'condition-incorrect-hostname-string
    'get-win-app-driver-host-uri)
  (make-condition-if                                           ; ポート番号を照合し，エラーがあればコンディションを発報する。
    (complement #'correct-port-number-p)
    ((session-data-port session))
    error
    'condition-incorrect-port-number
    'get-win-app-driver-host-uri)
  (concatenate                                                 ; ホストURIを生成する。
    'string                                                    ; 構造体sessionのhostとportを連結する。
    (session-data-host session)
    ":"                                                        ; ポート番号の前のコロン。
    (write-to-string                                           ; ポート番号の型はintegerなので文字列に変換する。
      (session-data-port session))))

(defun generate-endpoint-uri (session &rest directories)
  (apply
    #'concatenate
    `(string
      "http://"
      ,(get-win-app-driver-host-uri session)
      ,@directories)))

;(defmacro send-command (session method-type endpoint &optional (content nil))
(defmacro send-command (session method-type endpoint &optional (content "{\"\": \"\"}"))
  (cond
    ((eq method-type :get)
     `(progn
        (ad-get
          (generate-endpoint-uri ,session ,@endpoint))))
    ((eq method-type :post)
     `(progn
        (ad-post
          (generate-endpoint-uri ,session ,@endpoint)
          ,content)))
    ((eq method-type :delete)
     `(progn
        (ad-delete
          (generate-endpoint-uri ,session ,@endpoint))))
    (t `(progn
          (error
            (make-condition
              'condition-unknown-http-method-error
              :caller 'send-command))))))

