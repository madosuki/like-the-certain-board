(in-package :cl-user)
(defpackage like-certain-board.db
  (:use :cl :sxql)
  (:import-from :like-certain-board.config
                :config
                :*default-penalty-time*
                :*default-max-length*
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
           :insert-user-table
           :get-user-table
           :update-user-table
           :insert-kakolog-table
           :change-max-of-thread-in-db
           :init-threads-table
           :create-thread-in-db
           :get-table-column-count
           :update-last-modified-date-of-thread
           :get-res-count
           :get-max-line
           :update-res-count-of-thread
           :user-table-struct
           :thread-table-struct
           :get-default-name-from-name
           :get-default-name-from-id
           ))
(in-package :like-certain-board.db)

(defun connection-settings (&optional (db :maindb))
  (cdr (assoc db (config :databases))))

(defun db (&optional (db :maindb))
  (apply #'connect-cached (connection-settings db)))

(defmacro with-connection (conn &body body)
  `(let ((*connection* ,conn))
     ,@body))

(defstruct user-table-struct
  (board-name "" :type string)
  (user-name "" :type string)
  (hash "" :type string)
  (create-date "" :type string)
  (latest-date "" :type string)
  (is-admin nil :type integer)
  (cap-text "" :type string))

(defstruct thread-table-struct
  (title "" :type string)
  (create-date "" :type string)
  (last-modified-date "" :type string)
  (res-count 1 :type integer)
  (unixtime 0 :type intger)
  (max 1000 :type integer))


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

(defun get-default-name-from-name (url-name)
  (with-connection (db)
    (retrieve-one
     (select :default-name (from :board-list)
             (where (:like :url-name url-name))))))

(defun get-default-name-from-id (id)
  (with-connection (db)
    (retrieve-one
     (select :default-name (from :board-list)
             (where (:= :id id))))))



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

(defun set-posted-count-from-db (table-name session date n
                                        &optional (is-penalty nil)
                                          (wait-time *default-penalty-time*))
  (with-connection (db)
    (execute
     (update (intern table-name)
             (set= :count n
                   :appearance_date date
                   :is_penalty (if is-penalty 1 0)
                   :wait_time wait-time)
             (where (:like :session_data session))))))

(defun insert-posted-from-db (table-name session date)
  (with-connection (db)
    (execute
     (insert-into (intern table-name)
                  (set= :session_data session
                        :appearance_date date
                        :is_penalty 0
                        :count 1
                        :wait_time *default-penalty-time*)))))

(defun delete-posted-from-db (session)
  (with-connection (db)
    (execute
     (delete-from :posted-table
                  (where (:= :session_data session))
                  (limit 1)))))

(defun init-threads-table ()
  (with-connection (db)
    (execute
     (create-table (:threads :if-exists-not t)
                   ((title :type 'text
                           :not-null t)
                    (create-date :type 'datetime
                                 :not-null t)
                    (last-modified-date :type 'datetime
                                        :not-null t)
                    (res-count :type 'integer
                               :not-null t
                               :default 1)
                    (unixtime :type 'integer
                              :primary-key t)
                    (max-line :type 'integer
                              :default *default-max-length*
                              :not-null t)
                    (board-id :type 'integer
                              :not-null t))))))


(defun get-user-table (board-name user-name)
  (with-connection (db)
    (retrieve-one
     (select :*
             (from :user_table)
             (where (:and (:like :user_name user-name) (:like :board_name board-name)))))))

(defun insert-user-table (user-data)
  (let ((user-name (user-table-struct-user-name user-data))
        (board-name (user-table-struct-board-name user-data))
        (hash (user-table-struct-hash user-data))
        (create-date (user-table-struct-create-date user-data))
        (latest-date (user-table-struct-latest-date user-data))
        (is-admin (user-table-struct-is-admin user-data))
        (cap-text (user-table-struct-cap-text user-data)))
    (with-connection (db)
      (execute
       (insert-into :user_table
                    (set=
                     :board_name board-name
                     :user_name user-name
                     :hash hash
                     :create_date create-date
                     :latest_date latest-date
                     :is_admin is-admin
                     :cap_text cap-text))))))

(defun update-user-table (board-name user-name date)
  (with-connection (db)
    (execute
     (update :user_table
             (set= :latest_date date)
             (where (:and (:like :user_name user-name) (:like :board_name board-name)))))))

(defun insert-kakolog-table (unixtime title board-id)
  (with-connection (db)
    (execute
     (insert-into :kakolog
                  (set=
                   :unixtime unixtime
                   :title title
                   :board-id board-id)))))

(defun change-max-of-thread-in-db (unixtime)
  (with-connection (db)
    (execute
     (update :thread
             (set= :max 10000)
             (where (:like :unixtime unixtime))))))

(defun create-thread-in-db (&key title date unixtime max-line board-id)
  (with-connection (db)
      (execute
       (insert-into :threads
                    (set= :title title
                          :create-date date
                          :last-modified-date date
                          :res-count 1
                          :unixtime unixtime
                          :max-line (if (or (null max-line) (< max-line *default-max-length*))
                                        *default-max-length*
                                        max-line)
                          :board-id board-id)))))

(defun get-table-column-count (table-name column)
  (with-connection (db)
    (retrieve-one
     (select (fields (:count (intern column)))
             (from (intern table-name))))))


(defun update-last-modified-date-of-thread (&key date key board-id)
  (with-connection (db)
    (execute
     (update :threads
             (set= :last-modified-date date)
             (where (:and (:like :unixtime key)
                          (:= :board-id board-id)))))))

(defun get-res-count (&key key)
  (with-connection (db)
    (retrieve-one
     (select :res-count
             (from :threads)
             (where (:like :unixtime key))))))

(defun get-max-line (&key key)
  (with-connection (db)
    (retrieve-one
     (select :max-line
             (from :threads)
             (where (:like :unixtime key))))))

(defun update-res-count-of-thread (&key key board-id)
  (let ((tmp (get-res-count :key key)))
    (with-connection (db)
      (execute
       (update :threads
               (set= :res-count (1+  (cadr tmp)))
               (where (:= (:like :unixtime key)
                          (:= :board-id board-id))))))))
