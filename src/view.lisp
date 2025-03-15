(in-package :cl-user)
(defpackage like-certain-board.view
  (:use :cl :cl-markup)
  (:import-from :like-certain-board.config
                :*template-directory*
                :*domain-name*
                :*http-root-path*
                :*https-root-path*)
  (:import-from :caveman2
                :*response*
                :response-headers)
  ;; (:import-from :compile-template*
  ;;               :render-template*
  ;;               :*template-package*)
  (:import-from :datafly
   :encode-json)
  (:import-from :uiop
   :read-file-string)
  (:import-from :like-certain-board.db
   :get-default-name-from-name)
  (:import-from :like-certain-board.utils
                :separate-numbers-from-key-for-kako)
  (:import-from :generate-like-certain-board-strings
                :convert-html-special-chars
                :shape-text)
  (:export :render-json
   :index-view
           :board-view
   :thread-view
           :time-restrict-view
   :login-view
           :write-result-view
   :notfound-view
           :kakolog-view
   :kakolog-list-view
           :about-page-view
   :confirm-page-view
           :create-user-view
   :user-list-view))
(in-package :like-certain-board.view)

;; (djula:add-template-directory *template-directory*)

;; (defparameter *template-registry* (make-hash-table :test 'equal))

;; (defun render (template-path &optional env)
;;   (let ((template (gethash template-path *template-registry*)))
;;     (unless template
;;       (setf template (djula:compile-template* (princ-to-string template-path)))
;;       (setf (gethash template-path *template-registry*) template))
;;     (apply #'djula:render-template*
;;            template nil
;;            env)))

(defun render-json (object)
  (setf (getf (response-headers *response*) :content-type) "application/json")
  (encode-json object))

;;
;; Execute package definition

;; (defpackage like-certain-board.djula
;;   (:use :cl)
;;   (:import-from :like-certain-board.config
;;                 :config
;;                 :appenv
;;                 :developmentp
;;                 :productionp)
;;   (:import-from :caveman2
;;                 :url-for))

;; (setf djula:*template-package* (find-package :like-certain-board.djula))

(defparameter *og-prefix* (format nil "og: ~A" *https-root-path*))
(defparameter *default-ogp-image-name* "ogp_image_sample.png")

(defmacro base-html (title url ogp-image-name &body body)
  `(html5 :lang "ja" :prefix *og-prefix*
    (:head
     (:meta :charset "utf-8")
     (:meta
      :name "viewport" :content "width=device-width,initial-scale=1")
     (raw (when ,url
            (markup (:meta :property "og:title"
                           :content ,title)
                    (:meta :property "og:type"
                           :content "website")
                    (:meta :property "og:url"
                           :content ,url)
                    (:meta :property "og:image"
                           :content (format nil "~A/images/~A"
                                            *https-root-path*
                                            (if,ogp-image-name
                                             ,ogp-image-name
                                             *default-ogp-image-name*))))))
     (:link :rel "stylesheet"
            :type "text/css"
            :media "screen"
            :href "/css/main.css")
     (:title ,title))
    ,@body))

(defmacro main-content (title board-url-name url ogp-image-name is-thread thread-key &body body)
  `(base-html ,title ,url ,ogp-image-name
              (:body
               (:header
                (:nav (:ul
                       (:li (:a :href "/")
                            "トップに戻る")
                       (:li (:a :href "/about"
                                "このサイトについて"))
                       (raw (when ,is-thread
                              (markup (:li (:a :href (format nil "/~A" ,board-url-name)
                                               "板に戻る")))))
                       (raw (when ,thread-key
                              (markup (:li (:a :href (format nil "/test/read.cgi/~A/~A" ,board-url-name ,thread-key)
                                               "スレッドに戻る")))))
                       (raw (when ,board-url-name
                              (markup (:li (:a :href (format nil "/~A/kakolog" ,board-url-name)
                                               "過去ログ倉庫"))))))))
               (:div :id "main"
                     ,@body))))

(defun index-view (board-list)
  (declare (type (list string) board-list))
  (main-content "板一覧" nil *https-root-path* nil nil nil
                (:div :id "board-list"
                      (:h1 "板一覧")
                      (:ul
                       (loop for i in board-list
                             collect (markup (:li (:a :href (format nil "/~A" (getf i :url-name))
                                                      (getf i :name)))))))))

(defun set-thread-table-row (item bbs is-login csrf-token)
  (declare (type string bbs)
           (type boolean is-login)
           (type string csrf-token))
  (markup
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
         (format nil "~A" (getf item :max-line)))
    (raw (when is-login
           (markup (:td :class "cell-spacing"
                        (:form :action (format nil "/~A/api/thread" bbs)
                               :method "POST"
                               (:input :name "key"
                                       :type "hidden"
                                       :value (format nil "~A" (getf item :unixtime)))
                               (:input :name "mode"
                                       :type "hidden"
                                       :value "delete")
                               (:input :name "_csrf_token"
                                       :type "hidden"
                                       :value csrf-token)
                               (:button :name "submit"
                                        :type "submit"
                                        "削除")))))))))




(declaim (inine create-thread-form))
(defun create-thread-form (bbs time default-name csrf-token)
  (declare (type string bbs)
           (type integer time)
           (type string default-name)
           (type string csrf-token))
  (html5  (:h2 :class "form"
               "新規スレッド作成フォーム")
          (:form :action "/test/bbs.cgi"
                 :method "POST"
                 (:ul :class "form"
                      (:li :class "form"
                           (:label :for "subject"
                                   :class "form"
                                   "スレッドタイトル："
                                   (:input :name "subject"
                                           :type "text"
                                           :value ""
                                           :class "form"
                                           :required t)))
                      (:li :class "form"
                           (:label :for "FROM"
                                   :class "form"
                                   "名前:"
                                   (:input :name "FROM"
                                           :type "text"
                                           :value default-name
                                           :class "form")))
                      (:li :class "form"
                           (:label :for "mail"
                                   :class "form"
                                   "メールアドレス:"
                                   (:input :name "mail"
                                           :type "text"
                                           :value ""
                                           :class "form")))
                      (:li :class "form"
                           (:label :for "max_line"
                                   :class "form"
                                   "最大行指定（＊1001〜10000まで有効．それより下を指定した場合，1000になり，超えた場合は10000として扱います．）："
                                   (:input :name "max_line"
                                           :type "number"
                                           :value "1000"
                                           :class "form")))
                      (:li :class "form"
                           (:label :for "MESSAGE"
                                   :class "form"
                                   "本文:"
                                   (:textarea :name "MESSAGE"
                                              :value ""
                                              :cols 60
                                              :rows 60
                                              :class "form"
                                              :required t
                                              nil)))
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
                 (:input :name "_csrf_token"
                         :type "hidden"
                         :value csrf-token)
                 (:input :name "submit"
                         :type "hidden"
                         :value "新規スレッド作成"))))


(defun board-view (&key is-login board-name bbs thread-list time csrf-token url)
  (declare (type boolean is-login)
           (type string board-name)
           (type string bbs)
           (type integer time)
           (type string csrf-token)
           (type string url))
  (let ((default-name (cadr (get-default-name-from-name bbs))))
    (main-content board-name bbs url nil nil nil
                  (if is-login
                      (raw (markup (:h2 "ログイン済み")))
                      "")
                  (:h1 :style "text-align: center;"
                       board-name)
                  (:h2 :id "thread-content-title"
                       "スレッド一覧")
                  (:table :id "thread-list"
                          (:tr
                           (:th "スレ名")
                           (:th "作成日時")
                           (:th "更新日時")
                           (:th "レス数")
                           (:th "最大行")
                           (when is-login
                             (raw (markup (:th "削除ボタン")))))
                          (loop for i in thread-list
                                collect (set-thread-table-row i bbs is-login csrf-token)))
                  (raw (create-thread-form bbs time default-name csrf-token)))))

(declaim (inline set-thread-row))
(defun set-thread-row (count item bbs is-login key csrf-token)
  (markup
   (:dl :id (format nil "~A" count)
        (when is-login
          (raw (markup (:form :action (format nil "/~A/api/line" bbs)
                              :method "POST"
                              (:input :name "line"
                                      :type "hidden"
                                      :value (format nil "~A" count))
                              (:input :name "key"
                                      :type "hidden"
                                      :value (format nil "~A" key))
                              (:input :name "_csrf_token"
                                      :type "hidden"
                                      :value csrf-token)
                              (:input :name "mode"
                                      :type "hidden"
                                      :value "delete")
                              (:button :name "submit"
                                       :type "submit"
                                       (format nil "~A番目の行を削除" count))))))
        (:dt
         (format nil "~A 名前：" count)
         (:font
          :color "#008800"
          (:b
           (raw (getf item :name)))
          (getf item :trip))
         (let ((id (getf item :id)))
           (if id
               (format nil "[~A] 投稿日：~A ~A" (getf item :email) (getf item :date) id)
               (format nil "[~A] 投稿日：~A" (getf item :email) (getf item :date)))))
        (:dd :class "thread_text"
             (raw (getf item :text))))))


(defun thread-view (&key title thread bbs key time csrf-token is-login url)
  (let ((default-name (cadr (get-default-name-from-name bbs))))
    (main-content title bbs url nil t nil
                  (:h1 :id "title"
                       (raw title))
                  (loop for i in thread
                        for count from 1 to (1+ (length thread))
                        collect (set-thread-row count i bbs is-login key csrf-token))
                  (:div  (:h2 :class "form"
                              "投稿フォーム")
                         (:form :action "/test/bbs.cgi"
                                :method "POST"
                                (:ul :class "form"
                                     (:li :class "form"
                                          (:label :for "FROM"
                                                  :class "form"
                                                  "名前："
                                                  (:input :name "FROM"
                                                          :class "form"
                                                          :type "text"
                                                          :value default-name)))
                                     (:li :class "form"
                                          (:label :for "mail"
                                                  :class "form"
                                                  "メールアドレス:"
                                                  (:input :name "mail"
                                                          :type "text"
                                                          :class "form"
                                                          :value "")))
                                     (:li :class "form"
                                          (:label :for "MESSAGE"
                                                  :class "form"
                                                  "本文："
                                                  (:textarea :class "form"
                                                             :name "MESSAGE"
                                                             :cols 60
                                                             :rows 60
                                                             :value ""
                                                             :required t
                                                             nil)))
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
                                (:input :name "_csrf_token"
                                        :value csrf-token
                                        :type "hidden")
                                (:input :name "submit"
                                        :type "hidden"
                                        :value "書き込む")))
                  (:footer :id "footer"
                           (:nav
                            (:ul
                             (:li
                              (:a :href (format nil "/~A" bbs)
                                  "板に戻る"))))))))


(defun time-restrict-view (&key bbs key mode mail url)
  (main-content "連投規制" bbs url nil t key
                (:div :id "time-restrict"
                 (:h1 :id "alert-title"
                      "連投規制")
                 (raw (cond ((eq mode :restrict)
                             (markup (:p "10秒経つまつで投稿できません．")))
                            ((eq mode :restrict-24)
                             (markup (:p "24時間経過するまで投稿できません．10回この画面が表示されるとBANされます．")))
                            (t
                             (markup (:p "BANされました")
                                     (:p (format nil
                                                 "~Aに宛てに解除申請してくだされば対応します．その際にグローバルIPアドレスの情報が必要になります．但し，悪質な場合は誠に申し訳ありませんが永久BANとなり解除申請に応じられません．"
                                                 mail)))))))
                (:footer :id "time-restrict"
                         (:nav
                          (raw (when key
                                 (markup (:a :href (format nil "/test/read.cgi/~A/~A" bbs key)
                                             :class "nav-item-in-footer"
                                             "スレッドに戻る"))))
                          (:a :href (format nil "/~A" bbs)
                              :class "nav-item-in-footer"
                              "板に戻る")))))


(defun login-view (&key board-name board-url-name csrf-token is-login url)
  (main-content board-name board-url-name url nil t nil
                (:h1 "ログインページ")
                (raw (cond ((eq is-login :logged-in)
                            (markup (:h2 "ログイン済みです")))
                           ((eq is-login :failed)
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
                               :value "login")
                       (:input :name "_csrf_token"
                               :type "hidden"
                               :value csrf-token))
                (:footer :id "footer"
                         (:nav
                          (:ul
                           (:li
                            (:a :href (format nil "/~A" board-url-name)
                                "板に戻る")))))))

(defun write-result-view (&key board-url-name key error-type message url)
  (main-content (cond ((eq error-type :write-error)
                       "書き込みエラー")
                      ((eq error-type :create-error)
                       "新規スレッド作成エラー")
                      (t "何かのエラー"))
                board-url-name
                url
                nil
                t
                key
                (:div :id "error-msg"
                      :style "text-align: center"
                      (:p message)
                      (raw (when board-url-name
                             (markup (:a :href (format nil "/~A" board-url-name)
                                         "板に戻る"))))
                      (raw (when key
                             (markup
                              (:a :href (format nil "/~A/~A" board-url-name key)
                                  "スレッドに戻る")))))))

(defun notfound-view ()
  (base-html "404 Not Found" nil nil
             (:style "#error { text-align: center; background-color: gray; height: 100vh;} #status { font-family: Times, serif; font-size: 10vw; } #message { font-family: Helvetica, sans-serif; font-size: 5vw; }")
             (:div :id "error"
                   (:div :id "status"
                         "404")
                   (:div :id "message"
                         "Not Found"))))


(defun kakolog-view (&key title html-path board-url-name first second key url)
  (main-content (format nil "過去ログ: ~A" title) nil url nil t key
                (:div :id "kakolog-thread"
                      (raw (read-file-string html-path))
                      (raw (when board-url-name
                             (markup (:footer :id "footer"
                                              (:nav
                                               (:ul
                                                (:li
                                                 (:a :href (format nil "/~A" board-url-name)
                                                     "板に戻る")
                                                 (:a :href (format nil "/~A/kakolog" board-url-name)
                                                     "過去ログ倉庫に戻る")
                                                 (:a :href (format nil "/~A/kako/~A/~A/~A.dat"
                                                                   board-url-name first second key)
                                                     :download (format nil "~A.dat" key)
                                                     "datをダウンロードする場合はこちら")))))))))))


(defun kakolog-list-view (board-name url &optional (data nil))
  (flet ((set-kakolog-list-table (columns)
           (let* ((unixtime (getf columns :unixtime))
                  (title (getf columns :title))
                  (separated (separate-numbers-from-key-for-kako unixtime)))
             (markup
              (:tr
               (:td :class "cell-spacing"
                    (:a :href (format nil "/~A/kako/~A/~A/~A.html"
                                      board-name (car separated) (cdr separated) unixtime)
                        title))
               (:td :class "cell-spacing"
                    unixtime))))))
    (main-content "過去ログ倉庫" nil url nil t nil
                  (:div :id "kakolog-list-contents"
                   (:h1 "過去ログ倉庫")
                   (if data
                       (raw (markup (:table :id "thread-list"
                                            (:tr
                                             (:th "スレ名")
                                             (:th "key"))
                                            (loop for i in data
                                                  collect (set-kakolog-list-table i)))))
                       (raw (markup (:p :style "margin-left: 1rem"
                                     "過去ログはありません"))))
                   (:footer :id "footer"
                            (:nav
                             (:ul
                              (:li
                               (:a :href (format nil "/~A" board-name)
                                   "板に戻る")))))))))

(defun about-page-view (url)
  (main-content "About" nil url nil nil nil
                (:p "アバウトページ")))


;; WIP add confirm of write page
(defun confirm-page-view (&key board-name url mode data csrf-token)
  (let ((title (if (eq mode :write)
                   "書き込み確認"
                   "スレッド作成確認"))
        (key (if (eq mode :write)
                 (cadr (member "key" data :test #'string=))
                 nil))
        (subject (if (eq mode :create)
                     (cadr (member "subject" data :test #'string=))
                     nil))
        (FROM (cadr (member "FROM" data :test #'string=)))
        (mail (cadr (member "mail" data :test #'string=)))
        (MESSAGE (cadr (member "MESSAGE" data :test #'string=)))
        (time (cadr (member "time" data :test #'string=))))
    (main-content title board-name url nil (if (eq mode :write) t nil) key
                  (:h1 title)
                  (:div :id "check-terms-of-use"
                        (:h2 "本当に送信してよろしいですか？")
                        (:p "送信する前に規約をお読みください．")
                        (:p "送信すれば規約を了解した上で書き込みしたと見做します．"))
                  (:div :id "confirm-data"
                   (:p (format nil "板名: ~A" board-name))
                   (when key
                     (raw (markup (:p (format nil "スレッドキー: ~A" key)))))
                   (when subject
                     (raw (markup (:p (format nil "スレッド名: ~A" subject)))))
                   (:p (format nil "名前: ~A" (convert-html-special-chars FROM)))
                   (:p (format nil "メール: ~A" (if (eq mail :NO-DATA)
                                                    ""
                                                    (convert-html-special-chars mail))))
                   (:p "本文：")
                   (:div :class "thread_text" (raw (shape-text (convert-html-special-chars MESSAGE)))))
                  (:form :action "/test/bbs.cgi"
                         :method "POST"
                         (:input :name "bbs"
                                 :value board-name
                                 :type "hidden")
                         (when key
                           (raw (markup (:input :name "key"
                                                :value key
                                                :type "hidden"))))
                         (when subject
                           (raw (markup (:input :name "subject"
                                                :value subject
                                                :type "hidden"))))
                         (:input :name "FROM"
                                 :value FROM
                                 :type "hidden")
                         (:input :name "mail"
                                 :value (if (eq mail :NO-DATA)
                                            ""
                                            mail)
                                 :type "hidden")
                         (:input :name "MESSAGE"
                                 :value MESSAGE
                                 :type "hidden")
                         (:input :name "time"
                                 :value time
                                 :type "hidden")
                         (:input :name "confirm"
                                 :value "t"
                                 :type "hidden")
                         (:input :name "_csrf_token"
                                 :type "hidden"
                                 :value csrf-token)
                         (:input :name "submit"
                                 :type "submit"
                                 :class "form"
                                 :value  (if (eq mode :write)
                                             "書き込む"
                                             "新規スレッド作成"))))))

(defun create-user-view (&key board-url-name board-name csrf-token)
  (main-content "ユーザー作成ページ"  board-url-name (format nil "~A/~A/create-user" *https-root-path* board-url-name) nil t nil
                (:h1  :style "text-align: center"
                      (format nil "~A: ユーザー作成ページ"
                              board-name))
                (:form :action (format nil "~A/~A/api/user" *https-root-path* board-url-name)
                       :method "POST"
                       (:ul :class "form"
                            (:li :class "form"
                                 (:label :for "user_name"
                                         :class "form"
                                         "ユーザーネーム："
                                         (:input :name "user_name"
                                                 :type "text"
                                                 :value ""
                                                 :class "form"
                                                 :required t)))
                            (:li :class "form"
                                   (:label :for "password"
                                         :class "form"
                                         "パスワード："
                                         (:input :name "password"
                                                 :type "text"
                                                 :value ""
                                                 :class "form"
                                                 :required t)))
                            (:li :class "form"
                                 (:label :for "cap_text"
                                         :class "form"
                                         "キャップ："
                                         (:input :name "cap_text"
                                                 :type "text"
                                                 :value ""
                                                 :class "form"
                                                 :required t)))
                            (:li :class "form"
                                 (:button :name "submit"
                                          :type "submit"
                                          :class "form"
                                          "作成")))
                       (:input :name "mode"
                               :input "text"
                               :value "create"
                               :type "hidden")
                       (:input :name "_csrf_token"
                               :input "text"
                               :value csrf-token
                               :type "hidden"))))


(defun user-list-view (&key board-name board-url-name user-list csrf-token)
  (main-content "ユーザーリスト" board-url-name (format nil "~A/~A/user-list" *https-root-path* board-url-name) nil t nil
                (:div :style "margin-left: 1rem"
                 (:ul
                  (loop for i in user-list
                        collect (markup (:li
                                         (:p (getf i :user-name))
                                         (:form :action (format nil "~A/~A/api/user" *https-root-path* board-url-name)
                                                :method "POST"
                                                (:input :name "mode"
                                                        :type "hidden"
                                                        :input "text"
                                                        :value "delete")
                                                (:input :name "user_name"
                                                        :type "hidden"
                                                        :input "text"
                                                        :value (getf i :user-name))
                                                (:input :name "_csrf_token"
                                                        :input "text"
                                                        :type "hidden"
                                                        :value csrf-token)
                                                (:button :name "submit"
                                                         :type "submit"
                                                         "削除")))))))))
