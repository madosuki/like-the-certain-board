
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
  (:export :render
   :render-json
           :index-view
           :board-view))
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
                 (:td :class "cell-spacing"
                      (:a :href (format nil "/test/read.cgi/~A/~A" bbs (getf item :unixtime))
                          (getf item :title)))
                 (:td :class "cell-spacing"
                      (getf item :create-date))
                 (:td :class "cell-spacing"
                      (getf item :last-modified-date))
                 (:td :class "cell-spacing"
                      (format nil "~A" (getf item :res-count)))
                 (:td :class "cell-spacing"
                      (format nil "~A" (getf item :max-line)))))
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

    ;; <h4 style="margin-top: 4em;">新規スレッド作成フォーム</h4>
    ;; <form action="/test/bbs.cgi" method="post">
    ;;     <ul class="form">
    ;;         <li class="form">
    ;;             <label for="subject" class="form">スレッドタイトル</label>
    ;;             <input name="subject" type="text" value="" class="form" required/>
    ;;         </li>
    ;;         <li class="form">
    ;;             <label for="FROM" class="form">名前:</label>
    ;;             <input name="FROM" type="text" value="名無しさん" class="form" required/>
    ;;         </li>

    ;;         <li class="form">
    ;;             <label for="mail" class="form">メールアドレス</label>
    ;;             <input name="mail" type="text" value="" class="form"/>
    ;;         </li>
    ;;         <li class="form">
    ;;             <label for="max_line" class="form">最大行指定（＊1001〜10000まで有効．それより下を指定した場合，1000になり，超えた場合は10000として扱います．）</label>
    ;;             <input name="max_line" type="number" value="1000" class="form"/>
    ;;         </li>            
    ;;         <li class="form">
    ;;             <label for="MESSAGE" class="form">本文:</label>
    ;;             <textarea cols="60" id="" name="MESSAGE" rows="10" class="form" required></textarea>
    ;;         </li>
    ;;         <li class="form">
    ;;             <button type="submit">新規スレッド作成</button>
    ;;         </li>
    ;;     </ul>
    ;;     <input name="bbs" type="hidden" value="{{ bbs }}"/>
    ;;     <input name="time" type="hidden" value="{{ time }}"/>
    ;;     <input name="submit" type="hidden" value="新規スレッド作成"/>
;; </form>

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
                                    "スレッドタイトル")
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
                                    "最大行指定（＊1001〜10000まで有効．それより下を指定した場合，1000になり，超えた場合は10000として扱います．")
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
                          :value time))))


(defun board-view (&key is-login board-name bbs thread-list time)
  (base-html board-name
             (:body
              (:div :id "main"
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
                            (:tr
                             (loop for i in thread-list
                                   collect (set-thread-table-row i bbs is-login))))
                    (raw (create-thread-form bbs time))))))

