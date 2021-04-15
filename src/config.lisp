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

(defparameter *db-path* (uiop:getenv "BOARD_DB_PATH"))

(defun set-db-settings ()
  (with-open-file (input (if *db-path*
                             (concatenate 'string *db-path* "/db.json")
                             "db.json")
                         :direction :input
                         :if-does-not-exist nil)
    (let ((parsed (json:decode-json input)))
      (let ((site-db-name (cdr (assoc :site-db-name parsed)))
            (user-name-in-db (cdr (assoc :user-name-in-db parsed)))
            (user-password-in-db (cdr (assoc :user-password-in-db parsed)))
            (db-hostname (cdr (assoc :db-hostname parsed)))
            (solt (cdr (assoc :solt parsed))))
        (when site-db-name
          (setq *site-db-name* site-db-name))
        (when user-name-in-db
          (setq *user-name-in-db* user-name-in-db))
        (when user-password-in-db
          (setq *user-password-in-db* user-password-in-db))
        (when db-hostname
          (setq *db-hostname* db-hostname))
        (when solt
          (setq *solt* solt))))))

(set-db-settings)

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
