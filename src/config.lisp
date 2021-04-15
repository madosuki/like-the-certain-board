(in-package :cl-user)
(defpackage like-certain-board.config
  (:use :cl :cffi :trivial-shell :uiop)
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
                             *db-path*
                             "db.txt")
                         :direction :input
                         :if-does-not-exist nil)
    (loop for line = (read-line input nil)
          for count from 1
          while (< count 5)
          do (case count
                 (1
                  (setq *site-db-name* line))
                 (2
                  (setq *user-name-in-db* line))
                 (3
                  (setq *user-password-in-db* line))
                 (4
                  (setq *db-hostname* line))
                 (5
                  (setq *solt* line)))))
  (when (find #\a (trivial-shell:shell-command "if [ -e /etc/alpine-release ]; then echo alpine; fi"))
    (setq *db-hostname* *docked-db-container-name*)))

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
