(in-package :cl-user)
(defpackage like-certain-board.config
  (:use :cl :cffi :trivial-shell :uiop :cl-json)
  (:import-from :envy
                :config-env-var
                :defconfig)
  (:export :config
           :*application-root*
           :*static-directory*
           :*default-max-length*
           :*1001*
           :*default-penalty-time*
           :*24-hour-seconds*
           :*9-hour-seconds*
           :*delete-message*
           :*session-login-key*
           :*session-admin-key*
           :*session-cap-text-key*
           :*session-last-post-seconds*
           :*max-thread-list*
           :*admin-ipaddr*
           :*admin-mailaddr*
           :*dat-path*
           :*kakolog-html-path*
           :*kakolog-dat-path*
           :*log-path*
           :*settings-path*
           :*https-root-path*
           :*http-root-path*
           :*domain-name*
           :*session-post-count-key*
           :*time-error-10sec-msg*
           :*time-error-24h-msg*
           :*confirmed-key*
           :appenv
           :developmentp
           :productionp))
(in-package :like-certain-board.config)

(setf (config-env-var) "APP_ENV")

(defparameter *application-root*   (asdf:system-source-directory :like-certain-board))
(defparameter *static-directory*   (merge-pathnames #P"static/" *application-root*))

(pushnew #P"/usr/lib" cffi:*foreign-library-directories*)

(defparameter *user-name-in-db* "")
(defparameter *user-password-in-db* "")
(defparameter *site-db-name* "")
(defparameter *db-hostname* "127.0.0.1")
(defparameter *docked-db-container-name* "db")
(defparameter *domain-name* "localhost")
(defparameter *specific-http-port* ":8080")
(defparameter *specific-https-port* ":8081")
(defparameter *http-root-path* (format nil "http://~A~A" *domain-name* *specific-http-port*))
(defparameter *https-root-path* (format nil "https://~A~A" *domain-name* *specific-https-port*))

(defparameter *settings-path* (uiop:getenv "BOARD_SETTINGS_PATH"))

(defvar *default-max-length* 1000)
(defvar *1001* "1001<><>Over Max Thread<> Reached max. Can't write to this thread. <>")
(defvar *default-penalty-time* 60)
(defvar *24-hour-seconds* 86400)
(defvar *9-hour-seconds* (* 9 60 60))
(defvar *delete-message* "削除されました<><>削除されました<> 削除されました <><>")
(defvar *session-login-key* "logged-in")
(defvar *session-last-post-seconds* "last-post-seconds")
(defvar *session-admin-key* "is-admin")
(defvar *session-cap-text-key* "cap-text")
(defvar *session-post-count-key* "post-count")
(defvar *max-thread-list* 10000)
;; (defvar *admin-ipaddr* "172.20.0.1")
(defvar *admin-ipaddr* "127.0.0.1")
(defvar *admin-mailaddr* "example@example.com")
(defvar *dat-path* (uiop:getenv "DAT_DIR_PATH"))
(defvar *kakolog-html-path* (uiop:getenv "KAKOLOG_HTML_DIR_PATH"))
(defvar *kakolog-dat-path* (uiop:getenv "KAKOLOG_DAT_DIR_PATH"))
(defvar *log-path* "")
(defvar *time-error-10sec-msg*
  "10秒経つまで投稿できません．何度かこの画面が出ると24時間に規制時間が延びます．")
(defvar *time-error-24h-msg*
  "24時間経つまで投稿できません．何度かこの画面が出るとBANとなり，管理者に連絡する必要があります．また悪質な場合は永久BANとなります．")
(defvar *confirmed-key* "confirmed")



(defun set-settings ()
  (with-open-file (input (if *settings-path*
                             (concatenate 'string *settings-path* "/settings.json")
                             "settings.json")
                         :direction :input
                         :if-does-not-exist nil)
    (let ((parsed (json:decode-json input)))
      (let ((site-db-name (cdr (assoc :site-db-name parsed)))
            (user-name-in-db (cdr (assoc :user-name-in-db parsed)))
            (user-password-in-db (cdr (assoc :user-password-in-db parsed)))
            (db-hostname (cdr (assoc :db-hostname parsed)))
            (default-name (cdr (assoc :default-name parsed)))
            (log-path (cdr (assoc :log-path parsed))))
        (when site-db-name
          (setq *site-db-name* site-db-name))
        (when user-name-in-db
          (setq *user-name-in-db* user-name-in-db))
        (when user-password-in-db
          (setq *user-password-in-db* user-password-in-db))
        (when db-hostname
          (setq *db-hostname* db-hostname))
        (when log-path
          (setq *log-path* log-path))))))

(set-settings)

(defconfig :common
    `(:databases ((:maindb :postgres
                   :host ,*db-hostname*
                   :port 5432
                   :database-name ,*site-db-name*
                   :username ,*user-name-in-db*
                   :password ,*user-password-in-db*))))

(defconfig |development|
  '())

(defconfig |production|
  '())

(defconfig |test|
  '())

(defun config (&optional key)
  (envy:config #.(package-name *package*) key))

(defun appenv ()
  (uiop:getenv (config-env-var #.(package-name *package*))))

(defun developmentp ()
  (string= (appenv) "development"))

(defun productionp ()
  (string= (appenv) "production"))
