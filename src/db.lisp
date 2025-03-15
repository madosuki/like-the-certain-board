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
           :check-exists-row
           :insert-user-table
           :get-user-table
           :delete-user
           :get-user-list
           :update-user-table
           :insert-kakolog-table
           :change-max-of-thread-in-db
           :create-thread-in-db
           :get-table-column-count
           :update-last-modified-date-of-thread
           :update-last-dates-of-thread
           :get-res-count
           :get-max-line
           :update-res-count-of-thread
           :user-table-struct
           :make-user-table-struct
           :thread-table-struct
           :make-thread-table-struct
           :get-default-name-from-name
           :get-default-name-from-id
           :update-time-restrict-count-and-last-unixtime
           :get-data-from-time-restrict
           :insert-to-time-restrict-table
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
  (board-id "" :type integer)
  (user-name "" :type string)
  (hash "" :type string)
  (create-date "" :type string)
  (latest-date "" :type string)
  (is-admin nil :type integer)
  (cap-text "" :type string)
  (salt "" :type string))

(defstruct thread-table-struct
  (title "" :type string)
  (create-date "" :type string)
  (last-modified-date "" :type string)
  (last-rise-date "" :type string)
  (res-count 1 :type integer)
  (unixtime 0 :type integer)
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
             (order-by (:desc :last-rise-date))
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
             (order-by (:desc :last-rise-date))))))

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

(defun check-exists-row (unixtime)
  (let ((tmp (with-connection (db)
               (retrieve-one
                (select :*
                        (from :threads)
                        (where (:= :unixtime unixtime)))))))
    (if tmp
        t
        nil)))


(defun get-user-table (board-id user-name)
  (with-connection (db)
    (retrieve-one
     (select :*
             (from :user-table)
             (where (:and (:like :user-name user-name) (:= :board-id board-id)))))))

(defun get-user-table-from-id (board-id user-id)
  (with-connection (db)
    (retrieve-one
     (select :*
             (from :user-table)
             (where (:and (:like :id user-id) (:= :board-id board-id)))))))


(defun get-user-list (board-id)
  (with-connection (db)
    (retrieve-all
     (select (fields :user-name :cap-text :is-admin :salt)
             (from :user-table)
             (where (:= :board-id board-id))))))


(defun insert-user-table (user-data)
  (let ((user-name (user-table-struct-user-name user-data))
        (board-id (user-table-struct-board-id user-data))
        (hash (user-table-struct-hash user-data))
        (create-date (user-table-struct-create-date user-data))
        (latest-date (user-table-struct-latest-date user-data))
        (is-admin (user-table-struct-is-admin user-data))
        (cap-text (user-table-struct-cap-text user-data))
        (salt (user-table-struct-salt user-data)))
    (with-connection (db)
      (execute
       (insert-into :user-table
                    (set=
                     :board-id board-id
                     :user-name user-name
                     :hash hash
                     :create-date create-date
                     :latest-date latest-date
                     :is-admin is-admin
                     :cap-text cap-text
                     :salt salt))))))

(defun update-user-table (board-id user-name date)
  (with-connection (db)
    (execute
     (update :user-table
             (set= :latest-date date)
             (where (:and (:like :user-name user-name) (:like :board-id board-id)))))))

(defun delete-user (board-id user-name)
  (with-connection (db)
    (execute
     (delete-from :user-table
                  (where (:and (:like :user-name user-name) (:like :board-id board-id)))))))

(defun delete-user-from-id (board-id user-id)
  (with-connection (db)
    (execute
     (delete-from :user-table
                  (where (:and (:like :id user-id) (:like :board-id board-id)))))))



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
                          :last-rise-date date
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
             (where (:and (:= :unixtime key)
                          (:= :board-id board-id)))))))

(defun update-last-dates-of-thread (&key date key board-id)
  (with-connection (db)
    (execute
     (update :threads
             (set= :last-modified-date date
                   :last-rise-date date)
             (where (:and (:= :unixtime key)
                          (:= :board-id board-id)))))))

(defun get-res-count (&key key)
  (with-connection (db)
    (retrieve-one
     (select :res-count
             (from :threads)
             (where (:= :unixtime key))))))

(defun get-max-line (&key key)
  (with-connection (db)
    (retrieve-one
     (select :max-line
             (from :threads)
             (where (:= :unixtime key))))))

(defun update-res-count-of-thread (&key key board-id)
  (let ((tmp (get-res-count :key key)))
    (with-connection (db)
      (execute
       (update :threads
               (set= :res-count (1+  (cadr tmp)))
               (where (:and (:= :unixtime key)
                            (:= :board-id board-id))))))))

(defun update-time-restrict-count-and-last-unixtime (&key ipaddr count last-unixtime penalty-count)
  (with-connection (db)
    (execute
     (update :time-restrict
             (set= :count count
                   :last-unixtime last-unixtime
                   :penalty-count penalty-count)
             (where (:like :ipaddr ipaddr))))))

(defun get-data-from-time-restrict (&key ipaddr)
  (with-connection (db)
    (retrieve-one
     (select (:count :last-unixtime :penalty-count)
             (from :time-restrict)
             (where (:like :ipaddr ipaddr))))))

(defun insert-to-time-restrict-table (&key ipaddr last-unixtime)
  (with-connection (db)
    (execute
     (insert-into :time-restrict
                  (set=
                   :count 1
                   :last-unixtime last-unixtime
                   :ipaddr ipaddr)))))

