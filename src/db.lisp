(in-package :cl-user)
(defpackage like-certain-board.db
  (:use :cl :sxql)
  (:import-from :like-certain-board.config
                :config
                :*default-penalty-time*
                :*max-thread-list*)
  (:import-from :datafly
                :*connection*
                :retrieve-all
                :retrieve-one
                :execute)
  (:import-from :cl-dbi
                :connect-cached)
  (:export :connection-settings
           :db
           :with-connection
   :get-board-list
           :get-a-board-name-from-id
   :get-a-board-name-from-name
           :get-thread-list
   :get-expired-thread-list
           :get-a-thread
   :get-kakolog-thread-list
           :get-a-kakolog-thread
   :get-thread-list-when-create-subject-txt
           :delete-thread
   :delete-expire-threads
           :check-exists-table
           :check-exist-row
))
(in-package :like-certain-board.db)

(defun connection-settings (&optional (db :maindb))
  (cdr (assoc db (config :databases))))

(defun db (&optional (db :maindb))
  (apply #'connect-cached (connection-settings db)))

(defmacro with-connection (conn &body body)
  `(let ((*connection* ,conn))
     ,@body))

(defun get-board-list ()
  (with-connection (db)
    (retrieve-all
     (select :* (from :board-list)))))

(defun get-a-board-name-from-id (id)
  (with-connection (db)
    (retrieve-one
     (select :* (from :board-list)
             (where (:= :id id))))))

(defun get-a-board-name-from-name (url-name)
  (with-connection (db)
    (retrieve-one
     (select :* (from :board-list)
             (where (:like :url-name url-name))))))


(defun get-thread-list (board-id)
  (with-connection (db)
    (retrieve-all
     (select :* (from :threads)
             (order-by (:desc :last-modified-date))
             (where (:= :board-id board-id))
             (limit *max-thread-list*)))))

(defun get-expired-thread-list (datetime board-id)
  (with-connection (db)
    (retrieve-all
     (select :* (from :threads)
             (where (:and (:> (:datediff datetime :last-modified-date)
                              180)
                          (:= :board-id board-id)))))))

(defun get-a-thread (unixtime board-id)
  (with-connection (db)
    (retrieve-one
     (select :* (from :threads)
             (where (:and (:= :unixtime unixtime)
                          (:= :board-id board-id)))))))

(defun get-kakolog-thread-list (board-id)
  (with-connection (db)
    (retrieve-all
     (select :* (from :kakolog)
             (order-by (:desc :unixtime))
             (where (:= :board-id board-id))
             (limit *max-thread-list*)))))

(defun get-a-kakolog-thread (unixtime board-id)
  (with-connection (db)
    (retrieve-one
     (select :* (from :kakolog)
             (where (:and (:= :unixtime unixtime)
                          (:= :board-id board-id)))))))


(defun get-thread-list-when-create-subject-txt (board-id)
  (with-connection (db)
    (retrieve-all
     (select (fields :title :res-count :unixtime)
             (from :threads)
             (where (:= :board-id board-id))
             (order-by (:desc :last-modified-date))))))

(defun delete-thread (key board-id)
  (with-connection (db)
    (execute
     (delete-from :threads
                  (where (:and (:= :unixtime key)
                               (:= :board-id board-id)))
                  (limit 1)))))

(defun delete-expire-threads (datetime)
  (with-connection (db)
    (execute
     (delete-from :threads
                  (where (:> (:datediff datetime :last-modified-date)
                             180))))))


(defun check-exists-table (table-name)
  (with-connection (db)
    (retrieve-one
     (select :*
             (from (intern table-name))))))

(defun check-exist-row (unixtime)
  (let ((tmp (with-connection (db)
               (retrieve-one
                (select :*
                        (from :threads)
                        (where (:like :unixtime unixtime)))))))
    (if tmp
        t
        nil)))

(defun get-posted-values (table-name session)
  (with-connection (db)
    (retrieve-one
     (select (fields :count :appearance_date :is_penalty :wait_time)
             (from (intern table-name))
             (where (:like :session_data session))))))

(defun set-posted-count-from-db (table-name session universal-time n
                                        &optional (is-penalty nil)
                                          (wait-time *default-penalty-time*))
  (let ((date (get-current-datetime universal-time)))
    (with-connection (db)
      (execute
       (update (intern table-name)
               (set= :count n
                     :appearance_date date
                     :is_penalty (if is-penalty 1 0)
                     :wait_time wait-time)
               (where (:like :session_data session)))))))

(defun insert-posted-from-db (table-name session universal-time)
  (let ((date (get-current-datetime universal-time)))
    (with-connection (db)
      (execute
       (insert-into (intern table-name)
                    (set= :session_data session
                          :appearance_date date
                          :is_penalty 0
                          :count 1
                          :wait_time *default-penalty-time*))))))

(defun delete-posted-from-db (session)
  (with-connection (db)
    (execute
     (delete-from :posted-table
                  (where (:= :session_data session))
                  (limit 1)))))
