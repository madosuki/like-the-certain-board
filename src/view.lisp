(in-package :cl-user)
(defpackage like-certain-board.view
  (:use :cl :cl-markup)
  (:import-from :like-certain-board.config
   :*template-directory*)
  (:import-from :caveman2
                :*response*
                :response-headers)
  (:import-from :djula
                :add-template-directory
                :compile-template*
                :render-template*
                :*djula-execute-package*)
  (:import-from :datafly
   :encode-json)
  (:import-from :uiop
                :read-file-string)
  (:export :render
           :render-json
           :index-view
           :board-view
           :thread-view
           :time-restrict-view
           :login-view
           :write-result-view
           :notfound-view
           :kakolog-view
           :kakolog-list-view))
(in-package :like-certain-board.view)

(djula:add-template-directory *template-directory*)

(defparameter *template-registry* (make-hash-table :test 'equal))

(defun render (template-path &optional env)
  (let ((template (gethash template-path *template-registry*)))
    (unless template
      (setf template (djula:compile-template* (princ-to-string template-path)))
      (setf (gethash template-path *template-registry*) template))
    (apply #'djula:render-template*
           template nil
           env)))

(defun render-json (object)
  (setf (getf (response-headers *response*) :content-type) "application/json")
  (encode-json object))


;;
;; Execute package definition

(defpackage like-certain-board.djula
  (:use :cl)
  (:import-from :like-certain-board.config
                :config
                :appenv
                :developmentp
                :productionp)
  (:import-from :caveman2
                :url-for))

(setf djula:*djula-execute-package* (find-package :like-certain-board.djula))

(defmacro base-html (title &body body)
  `(html5
    (:head
     (:meta :charset "utf-8")
     (:meta
      :name "viewport" :content "width=device-width,initial-scale=1")
     (:link :rel "stylesheet"
            :type "text/css"
            :media "screen"
            :href "/css/main.css")
     (:title ,title))
    ,@body))

(defun index-view ()
  (base-html "Index"
             (:body
              (:h1 "Index"))))

(defun set-thread-table-row (item bbs is-login)
  (let ((normal (markup
                 (:tr
                  (:td :class "cell-spacing"
                       (:a :href (format nil "/test/read.cgi/~A/~A" bbs (getf item :unixtime))
                           (raw (getf item :title))))
                  (:td :class "cell-spacing"
                       (getf item :create-date))
                  (:td :class "cell-spacing"
                       (getf item :last-modified-date))
                  (:td :class "cell-spacing"
                       (format nil "~A" (getf item :res-count)))
                  (:td :class "cell-spacing"
                       (format nil "~A" (getf item :max-line))))))
        (special
          (if is-login
              (markup (:td :class "cell-spacing"
                           (:form :action (format nil "/~A/api/thread" bbs)
                                  :method "POST"
                                  (:input :name "key"
                                          :type "hidden"
                                          :value (format nil "~A" (getf item :unixtime)))
                                  (:input :name "mode"
                                          :type "hidden"
                                          :value "delete")
                                  (:button :name "submit"
                                           :type "submit"
                                           "削除"))))
              "")))
    (concatenate 'string normal special)))


(defmacro main-content (title &body body)
  `(base-html ,title
              (:body
               (:header
                (:nav (:ul
                       (:li (:a :href "/")
                            "トップに戻る")
                       (:li (:a :href "/about"
                                "このサイトについて")))))
               (:div :id "main"
                     ,@body))))

(declaim (inine create-thread-form))
(defun create-thread-form (bbs time)
  (html5  (:h4 :style "margin-top: 4em;"
                "新規スレッド作成フォーム")
           (:form :action "/test/bbs.cgi"
                  :method "POST"
                  (:ul :class "form"
                       (:li :class "form"
                            (:label :for "subject"
                                    :class "form"
                                    "スレッドタイトル：")
                            (:input :name "subject"
                                    :type "text"
                                    :value ""
                                    :class "form"
                                    :required t))
                       (:li :class "form"
                            (:label :for "FROM"
                                    :class "form"
                                    "名前:")
                            (:input :name "FROM"
                                    :type "text"
                                    :value ""
                                    :class "form"))
                       (:li :class "form"
                            (:label :for "mail"
                                    :class "form"
                                    "メールアドレス:")
                            (:input :name "mail"
                                    :type "text"
                                    :value ""
                                    :class "form"))
                       (:li :class "form"
                            (:label :for "max_line"
                                    :class "form"
                                    "最大行指定（＊1001〜10000まで有効．それより下を指定した場合，1000になり，超えた場合は10000として扱います．）：")
                            (:input :name "max_line"
                                    :type "number"
                                    :value "1000"
                                    :class "form"))
                       (:li :class "form"
                            (:label :for "MESSAGE"
                                    :class "form"
                                    "本文:")
                            (:textarea :name "MESSAGE"
                                       :value ""
                                       :cols 60
                                       :rows 10
                                       :class "form"
                                       :required t
                                       nil))
                       (:li :class "form"
                            (:button
                             :type "submit"
                             "新規スレッド作成")))
                  (:input :name "bbs"
                          :type "hidden"
                          :value bbs)
                  (:input :name "time"
                          :type "hidden"
                          :value time)
                  (:input :name "submit"
                          :type "hidden"
                          :value "新規スレッド作成"))))


(defun board-view (&key is-login board-name bbs thread-list time)
  (main-content board-name
                (if is-login
                    (:h2 "ログイン済み")
                    "")
                (:h1 :style "text-align: center;"
                     board-name)
                (:h4 :id "thread-content-title"
                     "スレッド一覧")
                (:table :id "thread-list"
                        (:tr
                         (:th "スレ名")
                         (:th "作成日時")
                         (:th "更新日時")
                         (:th "レス数")
                         (:th "最大行")
                         (when is-login
                           (:th "削除ボタン")))
                        (loop for i in thread-list
                              collect (set-thread-table-row i bbs is-login)))
                (raw (create-thread-form bbs time))))

(declaim (inline set-thread-row))
(defun set-thread-row (count item is-login key)
  (markup
   (:dl :id (format nil "~A" count)
        (when is-login
          (:form :action (format nil "/~A/api/line" bbs)
                 :method "POST"
                 (:input :name "line"
                         :type "hidden"
                         :value (format nil "~A" count))
                 (:input :name "key"
                         :type "hidden"
                         :value (format nil "~A" key))
                 (:button :name "submit"
                          :type "submit"
                          (format nil "~A番目の行を削除" count))))
        (:dt
         (format nil "~A 名前：" count)
         (:font
          :color "#008800"
          (:b
           (raw (getf item :name)))
          (getf item :trip))
         (format nil "[~A] 投稿日：~A ~A" (getf item :email) (getf item :date) (getf item :id)))
        (:dd :class "thead_text"
             (raw (getf item :text))))))


(defun thread-view (&key title thread bbs key time is-login)
  (main-content title
                (:h1 :id "title"
                     (raw title))
                (loop for i in thread
                      for count from 1 to (1+ (length thread))
                      collect (set-thread-row count i is-login key))
                (:form :action "/test/bbs.cgi"
                       :method "POST"
                       (:ul :class "form"
                            (:li :class "form"
                                 (:label :for "FROM"
                                         :class "form"
                                         "名前：")
                                 (:input :name "FROM"
                                         :class "form"
                                         :type "text"
                                         :value "名無しさん"))
                            (:li :class "form"
                                 (:label :for "mail"
                                         :class "form"
                                         "メールアドレス:")
                                 (:input :name "mail"
                                         :type "text"
                                         :class "form"
                                         :value ""))
                            (:li :class "form"
                                 (:label :for "MESSAGE"
                                         :class "form"
                                         "本文：")
                                 (:textarea :class "form"
                                            :name "MESSAGE"
                                            :cols 60
                                            :rows 10
                                            :value ""
                                            :required t
                                            nil))
                            (:li :class "form"
                                 (:button :type "submit"
                                          "送信")))
                       (:input :name "bbs"
                               :value bbs
                               :type "hidden")
                       (:input :name "key"
                               :value (format nil "~A" key)
                               :type "hidden")
                       (:input :name "time"
                               :value (format nil "~A" time)
                               :type "hidden")
                       (:input :name "submit"
                               :type "hidden"
                               :value "書き込む"))
                (:footer :id "footer"
                 (:nav
                  (:ul
                   (:li
                    (:a :href (format nil "/~A" bbs)
                        "板に戻る")))))))


(defun time-restrict-view (&key ipaddr bbs key minute mail)
  (main-content "連投規制"
                (:h1 :id "alert-title"
                     "投稿規制")
                (:p (format nil "Your IP Address: ~A" ipaddr))
                (raw (if (<= minute 1440)
                         (markup (:p (format nil "~A分経つまつで投稿できません．" minute)))
                         (progn
                           (markup (:p "BANされました")
                                   (:p (format nil
                                               "~AかTwitter公式アカウントにIPアドレスを記載して解除申請してくだされば対応します．但し，悪質な場合は永久BANとなり解除申請に応じられませんのであしからず．"
                                               mail))))))
                (:footer :id "time-restrict"
                         (:nav
                          (raw (when key
                                 (markup (:a :href (format nil "/test/read.cgi/~A/~A" bbs key)
                                             :class "nav-item-in-footer"
                                             "スレッドに戻る"))))
                          (:a :href (format nil "/~A" bbs)
                              :class "nav-item-in-footer"
                              "板に戻る")))))


(defun login-view (&key board-name board-url-name is-login)
  (main-content board-name
                (:h1 "ログインページ")
                (raw (cond ((eq is-login 'logged-in)
                            (markup (:h2 "ログイン済みです")))
                           ((eq is-login 'failed)
                            (markup (:h2 "ログインに失敗しました")))))
                (:form :action (format nil "/~A/api/user" board-url-name)
                       :method "POST"
                       (:ul :class "form"
                            (:li :class "form"
                                 (:label :for "user_name"
                                         :class "form"
                                         "User Name:")
                                 (:input :name "user_name"
                                         :type "text"
                                         :class "form"
                                         :pattern "^[a-zA-Z0-9]+"))
                            (:li :class "form"
                                 (:label :for "password"
                                         :class "form"
                                         "Your Password:")
                                 (:input :name "password"
                                         :class "form"
                                         :type "text"
                                         :pattern "^[a-zA-Z0-9]+"))
                            (:li :class "form"
                                 (:button :type "submit"
                                          "送信")))
                       (:input :name "mode"
                               :type "hidden"
                               :value "login"))
                (:footer :id "footer"
                         (:nav
                          (:ul
                           (:li
                            (:a :href (format nil "/~A" board-url-name)
                                "板に戻る")))))))

(defun write-result-view (&key board-url-name key error-type message)
  (main-content (cond ((eq error-type 'write-error)
                       "書き込みエラー")
                      ((eq error-type 'create-error)
                       "新規スレッド作成エラー")
                      (t "何かのエラー"))
                (:div :id "error-msg"
                      (:p message)
                      (raw (when board-url-name
                             (markup (:a :href (format nil "/~A" board-url-name)
                                         "板に戻る"))))
                      (raw (when key
                             (markup
                              (:a :href (format nil "/~A/~A" board-url-name key)
                                  "スレッドに戻る")))))))

(defun notfound-view ()
  (base-html "404 Not Found"
             (:style "#error { text-align: center; background-color: gray; height: 100vh;} #status { font-family: Times, serif; font-size: 10vw; } #message { font-family: Helvetica, sans-serif; font-size: 5vw; }")
             (:div :id "error"
                   (:div :id "status"
                         "404")
                   (:div :id "message"
                         "Not Found"))))


(defun kakolog-view (title html-path board-url-name)
  (main-content (format nil "過去ログ: ~A" title)
                (raw (read-file-string html-path))
                (raw (when board-url-name
                       (markup (:footer :id "footer"
                                        (:nav
                                         (:ul
                                          (:li
                                           (:a :href (format nil "/~A" board-url-name)
                                               "板に戻る")
                                           (:a :href (format nil "/~A/kakolog" board-url-name)
                                               "過去ログ倉庫に戻る"))))))))))


(defun kakolog-list-view (board-name &optional (data nil))
  (flet ((set-kakolog-list-table (columns)
           (let ((unixtime (getf columns :unixtime))
                 (title (getf columns :title)))
             (markup
              (:tr
               (:td :class "cell-spacing"
                    (:a :href (format nil "/~A/kakolog/~A" board-name unixtime)
                        title))
               (:td :class "cell-spacing"
                    unixtime))))))
    (main-content "過去ログ倉庫"
                  (:h1 "過去ログ倉庫")
                  (:div :class "thread"
                        (if data
                            (raw (markup (:table :id "thread-list"
                                                 (:tr
                                                  (:th "スレ名")
                                                  (:th "key"))
                                                 (loop for i in data
                                                       collect (set-kakolog-list-table i)))))
                            "過去ログはありません")))))
