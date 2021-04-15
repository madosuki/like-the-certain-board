(in-package :cl-user)
(defpackage like-certain-board.config
  (:use :cl :cffi :trivial-shell :uiop :cl-json)
  (:import-from :envy
                :config-env-var
                :defconfig)
  (:export :config
           :*application-root*
           :*static-directory*
           :*template-directory*
           :*solt*
           :*board-name*
           :*default-name*
           :*default-max-length*
           :*1001*
           :*default-penalty-time*
           :*24-hour-seconds*
           :*9-hour-seconds*
           :*delete-message*
           :*session-login-key*
           :*session-admin-key*
           :*session-cap-text-key*
           :*max-thread-list*
           :*admin-ipaddr*
           :*dat-path*
           :appenv
           :developmentp
           :productionp))
(in-package :like-certain-board.config)

(setf (config-env-var) "APP_ENV")

(defparameter *application-root*   (asdf:system-source-directory :like-certain-board))
(defparameter *static-directory*   (merge-pathnames #P"static/" *application-root*))
(defparameter *template-directory* (merge-pathnames #P"templates/" *application-root*))

(pushnew #P"/usr/lib" cffi:*foreign-library-directories*)

(defparameter *user-name-in-db* "")
(defparameter *user-password-in-db* "")
(defparameter *site-db-name* "")
(defparameter *db-hostname* "127.0.0.1")
(defparameter *docked-db-container-name* "db")
(defparameter *solt* "")

(defparameter *settings-path* (uiop:getenv "BOARD_SETTINGS_PATH"))

(defvar *default-name* "名無しさん")
(defvar *board-name* "testboard")
(defvar *default-max-length* 1000)
(defvar *1001* "1001<><>Over Max Thread<> Reached max. Can't write to this thread. <>")
(defvar *default-penalty-time* 60)
(defvar *24-hour-seconds* 86400)
(defvar *9-hour-seconds* (* 9 60 60))
(defvar *delete-message* "削除されました<><>削除されました<> 削除されました <>")
(defvar *session-login-key* "logged-in")
(defvar *session-admin-key* "is-admin")
(defvar *session-cap-text-key* "cap-text")
(defvar *max-thread-list* 10000)
(defvar *admin-ipaddr* "127.0.0.1")
(defvar *dat-path* "dat/")


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
            (solt (cdr (assoc :solt parsed)))
            (default-name (cdr (assoc :default-name parsed)))
            (board-name (cdr (assoc :board-name parsed))))
        (when site-db-name
          (setq *site-db-name* site-db-name))
        (when user-name-in-db
          (setq *user-name-in-db* user-name-in-db))
        (when user-password-in-db
          (setq *user-password-in-db* user-password-in-db))
        (when db-hostname
          (setq *db-hostname* db-hostname))
        (when solt
          (setq *solt* solt))
        (when default-name
          (setq *default-name* default-name))
        (when board-name
          (setq *board-name* board-name))))))

(set-settings)

(defconfig :common
    `(:databases ((:maindb :mysql
                   :host ,*db-hostname*
                   :port 3306
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
