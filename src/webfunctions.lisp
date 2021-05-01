(in-package :cl-user)
(defpackage like-certain-board.webfunctions
  (:use :cl
        :caveman2
        :like-certain-board.config
        :like-certain-board.view
        :like-certain-board.db
        :datafly
        :sxql
        :quri
        :cl-fad
        :generate-like-certain-board-strings)
  (:export
   :put-thread-list
   :to-kakolog
   :bbs-cgi-function
   :get-value-from-key
   :check-exist-row
   :check-abuse-post
   :delete-thread
   :set-response-status
   :create-safety-strings
   :login
   :create-user
   :get-thread-list-when-create-subject-txt
   :generate-dat-name))
(in-package :like-certain-board.webfunctions)

(deftype mysql-true-type (n) `(= n 1))
(deftype mysql-false-type (n) `(= n 0))
(defvar *mysql-true* 1)
(defvar *mysql-false* 0)

(defvar *posted-table* "posted_table")

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

(defmacro caddddr (v)
  `(caddr (cddr ,v)))

(defmacro cadddddr (v)
  `(caddr (cdddr ,v)))

(defmacro regex-group-bind (var-list regex-string target-string &body body)
  `(cl-ppcre:register-groups-bind (,@var-list)
                                  (,regex-string ,target-string)
                                  ,@body))
(defmacro escape-string (text)
  `(escape-sql-query ,text))

(defmacro set-response-status (status)
  `(setf (response-status *response*) ,status))


;; Functions

(defun flatten (a &optional (result (list nil)))
  (if a
      (let ((base (car a)))
        (let ((l (car base))
              (r (cdr base)))
          (if result
              (setq result (list r l))
              (progn
                (push l result)
                (push r result)))
          (flatten (cdr a) result)))
      (reverse result)))

(defun get-session-from-cookie (request)
  (let ((cookie (gethash "cookie" (request-headers *request*))))
    (unless cookie
      (return-from get-session-from-cookie nil))
    (let ((splited-cookie (flatten (mapcar #'(lambda (v)
                                               (let  ((base (cl-ppcre:split "=" v)))
                                                 (cons (car base) (cadr base))))
                                           (cl-ppcre:split ";" cookie)))))
      (when (<= (length splited-cookie) 1)
        (format t "~%not reach latest length cookie~%")
        (return-from get-session-from-cookie nil))
      (let ((session (member "lack.session" splited-cookie :test #'equal)))
        (if session
            (cadr session)
            nil)))))


(defun change-line-in-dat (filename outpath text target-number)
  (let ((result ""))
    (with-open-file (in filename
                        :direction :input
                        :external-format :CP932)
      (do ((line (read-line in) (read-line in nil 'eof))
           (count 1 (incf count)))
          ((eq line 'eof))
        (setq result (concatenate 'string
                                  result
                                  (format nil "~A~%" (if (= target-number count)
                                                         text
                                                         line))))))
    (with-open-file (out outpath
                         :direction :output
                         :if-exists :supersede
                         :element-type '(unsigned-byte 8))
      (write-sequence (sb-ext:string-to-octets result :external-format :CP932) out))))

(defun delete-line-in-dat (key line-number)
  (let* ((path (concatenate 'string *dat-path* key ".dat"))
         (outpath (concatenate 'string path ".out"))
         (is-not-error nil))
    (handler-case (change-line-in-dat path outpath *delete-message* line-number)
      (error (e)
        (format t "~%~A~%" e))
      (:no-error (c)
        (declare (ignore c))
        (rename-file outpath path)
        (setq is-not-error t)))
    is-not-error))

(defun get-thread-list ()
  (with-connection (db)
    (retrieve-all
     (select :* (from :threads)
             (order-by (:desc :last-modified-date))
             (limit *max-thread-list*)
             (where (:like :is-deleted *mysql-false*))))))

(defun get-thread-list-when-create-subject-txt ()
  (with-connection (db)
    (retrieve-all
     (select (fields :title :res-count :unixtime)
             (from :threads)
             (order-by (:desc :last-modified-date))))))

(defun delete-thread (key)
  (with-connection (db)
    (execute
     (delete-from :threads
                  (where (:= :unixtime key))
                  (limit 1)))))


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


(defun get-detail-time-from-universal-time (time &optional (is-utc t))
  (multiple-value-bind (second minute hour date month year day summer timezone)
      (if is-utc (decode-universal-time time) (decode-universal-time time 9))
    (declare (ignore day summer timezone))
    (list :year year :month month :date date :hour hour :minute minute :second second)))

(defmacro hour-to-second (h)
  `(* ,h 60 60))

;; (defun check-abuse-post (session time)
;;   (unless session
;;     (return-from check-abuse-post t))
;;   (let* ((table-name *posted-table*)
;;          (fetch-result (get-posted-values table-name session))
;;          (current-detail-date (get-detail-time-from-universal-time time))
;;          (current-second (getf current-detail-date :second))
;;          (current-minute (getf current-detail-date :minute))
;;          (current-hour (getf current-detail-date :hour))
;;          (current-date (getf current-detail-date :date)))
;;     (unless fetch-result
;;       (insert-posted-from-db table-name session time)
;;       (return-from check-abuse-post t))
;;     (let* ((appearance-date (getf fetch-result :appearance-date))
;;            (is-penalty (getf fetch-result :is-penalty))
;;            (count (getf fetch-result :count))
;;            (appearance-detail-date (get-detail-time-from-universal-time appearance-date))
;;            (target-second (getf appearance-detail-date :second))
;;            (target-minute (getf appearance-detail-date :minute))
;;            (target-hour (getf appearance-detail-date :hour))
;;            (target-date (getf appearance-detail-date :date))
;;            (wait-time (getf fetch-result :wait-time))
;;            (left  (+ current-second (hour-to-second current-hour) (* current-minute 60)))
;;            (right (+ target-second (hour-to-second target-hour) (* target-minute 60)))
;;            (diff (abs (- (if (< target-date current-date) (+ left *24-hour-seconds*) left) right))))
;;       (when (>= wait-time *24-hour-seconds*)
;;         (return-from check-abuse-post nil))
;;       (cond ((<= wait-time diff)
;;              (setq is-penalty nil)
;;              (set-posted-count-from-db table-name session time 0)
;;              t)
;;             ((> count 5)
;;              (set-posted-count-from-db
;;               table-name session time (1+ count) t
;;               (if (< wait-time *24-hour-seconds*) (+ wait-time *default-penalty-time*) wait-time))
;;              nil)
;;             (t
;;              (set-posted-count-from-db table-name session time (1+ count) nil wait-time)
;;              nil)))))

(defun check-abuse-post (before-unixtime current-unixtime)
  (unless before-unixtime
    (return-from check-abuse-post t))
  (if (< (- current-unixtime before-unixtime) 60)
      nil
      t))

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
                    (is-deleted :type 'integer
                                :default *mysql-false*))))))


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

(defun format-datetime (date)
  (multiple-value-bind (second minute hour date month year day summer timezone)
      (decode-universal-time date)
    (declare (ignore day summer timezone))
    (format nil "~A/~2,'0d/~2,'0d ~2,'0d:~2,'0d:~2,'0d" year month date hour minute second)))

(defun get-table-column-count (table-name column)
  (with-connection (db)
    (retrieve-one
     (select (fields (:count (intern column)))
             (from (intern table-name))))))

(defun check-exists-threads-table-and-create-table-when-does-not ()
  (handler-case (check-exists-table "threads")
    (error (e)
      (declare (ignore e))
      (init-threads-table))))

(defmacro get-value-from-key (key target)
  `(cdr (assoc ,key ,target :test #'string=)))

(defmacro get-value-from-key-on-list (key target)
  `(cadr (member ,key ,target :test #'string=)))

(defmacro generate-dat-name (l)
  `(concatenate 'string (write-to-string (cadr (member :unixtime ,l))) ".dat"))

(defmacro create-safety-strings (s)
  `(replace-not-available-char-when-cp932 (escape-sql-query ,s)))


(defun create-dat (&key unixtime first-line)
  (let* ((filename (concatenate 'string (write-to-string unixtime) ".dat"))
         (path (concatenate 'string *dat-path* filename)))
    (unless (cl-fad:directory-exists-p *dat-path*)
      (ensure-directories-exist *dat-path*))
    (with-open-file (i path
                       :direction :output
                       :if-does-not-exist :create
                       :element-type '(unsigned-byte 8))
      (let ((tmp (sb-ext:string-to-octets first-line :external-format :sjis)))
        (write-sequence tmp i)))))

(defun create-res (&key name trip-key email date text ipaddr (first nil) (title ""))
  (let* ((datetime (replace-hyphen-to-slash (get-current-datetime date)))
         (id (generate-id :ipaddr ipaddr :date datetime :solt *solt*))
         (trip (if (and (stringp trip-key) (string/= trip-key ""))
                   (generate-trip (subseq trip-key 1 (length trip-key)) "utf8")
                   ""))
         (final-text
           (apply-dice
            (create-safety-strings
             (shape-text
              (replace-other-line-to-lf text))))) ;; did convert-html-special-chars in shape-text function
         (mail (create-safety-strings (convert-html-special-chars email)))
         (final-name (create-safety-strings (convert-html-special-chars name))))
    (when (string/= trip "")
      (setq trip (concatenate 'string "</b>" (string #\BLACK_DIAMOND) trip "<b>")))
    (if first
        (format nil "~A~A<>~A<>~A ID:~A<>~A<>~A~%" (apply-dice final-name t) trip mail datetime id final-text title)
        (format nil "~A~A<>~A<>~A ID:~A<>~A<>~%" (apply-dice final-name t) trip mail datetime id final-text))))

(defun create-thread-in-db (&key title create-date unixtime max-line)
  (let ((date (get-current-datetime create-date)))
    (handler-case (with-connection (db)
                      (execute
                       (insert-into :threads
                                    (set= :title title
                                          :create-date date
                                          :last-modified-date date
                                          :res-count 1
                                          :unixtime unixtime
                                          :max-line (if (or (null max-line) (< max-line *default-max-length*))
                                                        *default-max-length*
                                                        max-line)))))
      (error (e)
        (format t "Error create-thread-in-db: ~A~%" e)))))

(defun change-max-of-thread-in-db (unixtime)
  (with-connection (db)
    (execute
     (update :thread
             (set= :max 10000)
             (where (:like :unixtime unixtime))))))

(defun decode-max-line-string (target)
  (let ((regex-str "max_line=([0-9]+)"))
    (regex-group-bind (number)
                      regex-str target
                      (cond ((null number)
                             nil)
                            ((= (length number) 4)
                             (parse-integer number :junk-allowed t))
                            ((<= 5 (length number))
                             *max-thread-list*)
                            (t
                             1000)))))


(defun create-thread (&key _parsed date ipaddr)
  (check-exists-threads-table-and-create-table-when-does-not)
  (let ((title (get-value-from-key-on-list "subject" _parsed))
        (name (get-value-from-key-on-list "FROM" _parsed))
        (trip-key "")
        (text (get-value-from-key-on-list "MESSAGE" _parsed))
        (email (get-value-from-key-on-list "mail" _parsed))
        (max-line (escape-string (get-value-from-key-on-list "max_line" _parsed)))
        (unixtime (get-unix-time date))
        (bbs (escape-string (get-value-from-key-on-list "bbs" _parsed)))
        (is-cap nil))
    (when (or (null bbs) (eq bbs 'no-data) (null text) (eq text 'no-data))
      (return-from create-thread 400))
    (when (and (gethash *session-login-key* *session*) (gethash *session-cap-text-key* *session*))
      (setq is-cap t))
    (when (eq email 'no-data)
      (setq email ""))
    (when (eq text 'no-data)
      (setq text ""))
    (when (eq name 'no-data)
      (setq name *default-name*))
    (if (stringp title)
        (progn
          (setq title (create-safety-strings (convert-html-special-chars title)))
          (let ((tmp (separate-trip-from-input name)))
            (when (> (length tmp) 1)
              (setq trip-key (cadr tmp)))
            (setq name (car tmp)))
          (when (string/= email "")
            (setq max-line (decode-max-line-string email)))
          (when (stringp max-line)
            (setq max-line (parse-integer max-line :junk-allowed t)))
          (if (numberp max-line)
              (cond ((< *max-thread-list* max-line)
                     (setq max-line *max-thread-list*)
                     (setq email "Expand maximum line done!"))
                    ((and (< *default-max-length* max-line) (<= max-line *max-thread-list*))
                     (setq email "Expand maximum line done!"))
                    (t
                     (setq max-line *default-max-length*)))
              (setq max-line *default-max-length*))
          (labels ((progress (title date unixtime ipaddr name text &optional (count 0))
                     (handler-case (funcall (lambda (title date unixtime ipaddr name text)
                                              (create-thread-in-db :title title
                                                                   :create-date date
                                                                   :unixtime unixtime
                                                                   :max-line max-line)
                                              (create-dat :unixtime unixtime
                                                          :first-line (create-res :name (if is-cap (gethash *session-cap-text-key* *session*) name) :trip-key trip-key :email email :text text :ipaddr ipaddr :date date :first t :title title))
                                              200)
                                            title date unixtime ipaddr name text)
                       (error (e)
                         (format t "~%Error in create-thread-function: ~A~%" e)
                         (incf unixtime)
                         (if (< count 10)
                             (progress title date unixtime ipaddr name text (incf count))
                             (return-from create-thread 400))))))
            (progress title date unixtime ipaddr name text)))
        400)))

(defun insert-res (_parsed ipaddr universal-time)
  (let* ((bbs (get-value-from-key-on-list "bbs" _parsed))
         (key (get-value-from-key-on-list "key" _parsed)) ;; unix time
         (time (get-value-from-key-on-list "time" _parsed)) ;; same above
         (from (get-value-from-key-on-list "FROM" _parsed)) ;; name
         (mail (get-value-from-key-on-list "mail" _parsed))
         (message (get-value-from-key-on-list "MESSAGE" _parsed))
         (status 200)
         (is-cap nil))
    (when (and (gethash *session-login-key* *session*) (gethash *session-cap-text-key* *session*))
      (setq is-cap t))
    (when (or (null from) (eq from 'no-data))
      (setq from *default-name*))
    (when (or (null mail) (eq mail 'no-data))
      (setq mail ""))
    (when (or (null bbs)
              (null key)
              (null message)
              (eq bbs 'no-data)
              (eq key 'no-data)
              (eq time 'no-data)
              (eq message 'no-data))
      (return-from insert-res 400))
    (when (string= from "")
      (setq from *default-name*))
    (when (and (string/= time "") (< (cadr (get-res-count :key key)) (cadr (get-max-line :key key)))
               (> (get-unix-time universal-time) (parse-integer time :radix 10)))
      (with-open-file (input (concatenate 'string *dat-path* key ".dat")
                             :direction :output
                             :if-exists :append
                             :if-does-not-exist nil
                             :element-type '(unsigned-byte 8)
                             :external-format :sjis)
        (let* ((tmp (separate-trip-from-input from))
               (name (car tmp))
               (trip (if (> (length tmp) 1)
                         (cadr tmp)
                         "")))
          (let ((res (create-res :name (if is-cap (gethash *session-cap-text-key* *session*) name) :trip-key trip :email mail :text message :ipaddr ipaddr :date universal-time)))
            (write-sequence (sb-ext:string-to-octets res :external-format :sjis) input)
            (update-last-modified-date-of-thread :date universal-time :key key)
            (update-res-count-of-thread :key key)
            (when (>= (cadr (get-res-count :key key)) (cadr (get-max-line :key key)))
              (write-sequence
               (sb-ext:string-to-octets *1001* :external-format :sjis) input)
              (update-res-count-of-thread :key key)
              (update-last-modified-date-of-thread :date universal-time :key key))
            (format t "~%~%insert!~%~%")))))
    status))

(defun put-thread-list (board-name web)
  (if (string= board-name *board-name*)
      (progn
        (check-exists-threads-table-and-create-table-when-does-not)
        (let ((result (get-thread-list))
              (is-login (gethash *session-login-key* *session*)))
          (dolist (x result)
            (setf (getf x :create-date) (format-datetime (getf x :create-date)))
            (setf (getf x :last-modified-date) (format-datetime (getf x :last-modified-date))))
          (board-view :board-name *board-title*
                      :bbs board-name
                      :time (get-unix-time (get-universal-time))
                      :thread-list result
                      :is-login is-login)
          ;; (render #P"board.html" (list :board-name *board-title*
          ;;                              :bbs board-name
          ;;                              :time (get-unix-time (get-universal-time))
          ;;                              :threads result
          ;;                              :is-login is-login))
          ))
      (on-exception web 404)))

(defun get-param (body)
  (let ((tmp (cl-ppcre:split "&" body))
        (result nil))
    (dolist (x tmp)
      (push (cl-ppcre:split "=" x) result))
    (nreverse result)))

(defun try-url-decode (x &optional (encode :UTF-8) (error-count 0))
  (format t "~%url-encode: ~A~%" x)
  (handler-case (quri:url-decode x :encoding encode)
    (error (e)
      (declare (ignore e))
      (case error-count
        (0
         (try-url-decode x :CP932 (1+ error-count)))
        (otherwise
         x)))))

(defun detect-decode (s &optional (encode :CP932) (error-count 0))
  (handler-case (cons (quri:url-decode s :encoding encode) error-count)
    (error (e)
      (declare (ignore e))
      (case error-count
        (0
         (detect-decode s :UTF-8 (1+ error-count)))
        (otherwise
         nil)))))

(defun update-last-modified-date-of-thread (&key date key)
  (with-connection (db)
    (execute
     (update :threads
             (set= :last-modified-date (get-current-datetime date))
             (where (:like :unixtime key))))))

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

(defun update-res-count-of-thread (&key key)
  (let ((tmp (get-res-count :key key)))
    (with-connection (db)
      (execute
       (update :threads
               (set= :res-count (1+  (cadr tmp)))
               (where (:like :unixtime key)))))))

(defun generate-confirme-page (bbs key is-utf8 form)
  (let* ((from (get-value-from-key-on-list "FROM" form))
         (message (get-value-from-key-on-list "MESSAGE" form))
         (mail (get-value-from-key-on-list "mail" form))
         (time (get-unix-time (get-universal-time)))
         (confirme-first "<html><!-- 2ch_X:cookie --><head><title>書き込み確認</title>")
         (html-charset (if is-utf8 "<meta charset=\"utf-8\">" "<meta http-equive=\"Conent-Type\" content=\"text/html; charset=x-sjis\">"))
         (confirme-body "</head><body>")
         (confirm-msg "<p>書き込みについて</p>")
         (confirme-form (concatenate 'string 
                                     "<form action=\"/test/bbs.cgi\" method=\"post\">"
                                     "<input name=\"FROM\" type=\"hidden\" value=\"" from "\">"
                                     "<input name=\"mail\" type=\"hidden\" value=\"" (if (null mail) "" mail) "\">"
                                     "<input name=\"MESSAGE\" type=\"hidden\" value=\"" MESSAGE "\">"
                                     "<input name=\"bbs\" type=\"hidden\" value=\"" bbs "\">"
                                     "<input name=\"key\" type=\"hidden\" value=\"" key "\">"
                                     "<input name=\"time\" type=\"hidden\" value=\"" (write-to-string time) "\">"
                                     "<input name=\"submit\" type=\"hidden\" value=\"" "上記全てを承諾して書き込む\">"
                                     "<button type=\"submit\">上記全てを承諾して書き込む</button>"
                                     "</form>"
                                     ))
         (confirme-end "</body></html>")
         (confirm-html (concatenate 'string confirme-first html-charset confirme-body confirm-msg confirme-form confirme-end)))
    (if is-utf8
        (sb-ext:string-to-octets confirm-html :external-format :UTF-8)
        (sb-ext:string-to-octets confirm-html :external-format :SJIS))))

(defun bbs-cgi-function (body-text ipaddr universal-time &optional (error-count 0))
  (let ((decoded-query (sb-ext:octets-to-string (coerce body-text '(vector (unsigned-byte 8)))
                                                 :external-format :UTF-8))
        (form nil))
    (if decoded-query
        (let ((parsed-param (get-param decoded-query)))
          (dolist (x parsed-param)
            (let ((key (car x))
                  (value (cadr x)))
              (if (null value)
                  (setq form (nconc (list key 'no-data) form))
                  (setq form (nconc (list key (try-url-decode value)) form)))))
          (let ((bbs (get-value-from-key-on-list "bbs" form))
                (key (get-value-from-key-on-list "key" form))
                (submit (get-value-from-key-on-list "submit" form)))
            (cond
              ((string= submit "書き込む")
               (cond ((or (null key) (null bbs))
                      (set-response-status 400)
                      (write-result-view :error 'unknown :message "bad parameter: require bbs and key params."))
                     ((> (cadr (get-res-count :key key)) *default-max-length*)
                      (set-response-status 503)
                      (write-result-view :error 'write-error :message "このスレッドはレス数が最大数に達しています．"))
                     (t
                      (let ((status (insert-res form ipaddr universal-time)))
                        (if (= status 200)
                            (progn
                              (setf (getf (response-headers *response*) :location) (concatenate 'string "/test/read.cgi/" bbs "/" key))
                              (set-response-status 302))
                            (progn
                              (set-response-status status)
                              (write-result-view :error 'write-error :message "混雑等の理由で新規スレッド作成に失敗しました．")))))))
              ((string= submit "新規スレッド作成")
               (if (null bbs)
                   (progn (set-response-status 400)
                          (write-result-view :error 'create-error :message "bad parameter: require bbs param."))
                   (let ((status (create-thread :_parsed form :date universal-time :ipaddr ipaddr)))
                     (if (= status 200)
                         (progn (setf (getf (response-headers *response*) :location) (concatenate 'string "/" bbs))
                                (set-response-status 302)
                                (next-route))
                         (progn
                           (set-response-status status)
                           (write-result-view :error-type 'create-error :message "混雑等の理由で新規スレッド作成に失敗しました．"))
                         ))))
              (t
               (set-response-status 400)
               (write-result-view :error-type 'unknown :message "bad parameter: does not exists that mode.")))))
        (progn
          (set-response-status 400)
          (write-result-view :error-type 'something :message "bad parameter: missing params.")))))

(defun load-file-with-recursive (pathname start end)
  (with-open-file (input pathname
                         :direction :input
                         :element-type '(unsigned-byte 8))
    (let ((file-size (file-length input)))
      (when (or (> 0 start) (< file-size end) (> 0 end))
               (return-from load-file-with-recursive nil))
      (let ((buf (make-array (if (= end 0)
                                 file-size
                                 end)
                             :element-type '(unsigned-byte 8)
                             :initial-element 0)))
        (read-sequence buf input :start start :end end)
        buf))))

(defun load-dat (pathname start end)
  (let ((tmp (load-file-with-recursive pathname 0 0)))
    (if (null tmp)
        (progn
          (set-response-status 404)
          nil))
    (handler-case (flexi-streams:octets-to-string tmp :external-format :utf-8)
      (error (e)
        (declare (ignore e))
        (flexi-streams:octets-to-string tmp)))))


(defun check-time-over-logged-in (board-name user-name universal-time)
  (let ((db-data (get-user-table board-name user-name)))
    (unless db-data
      (return-from check-time-over-logged-in 'not-found))
    (let ((latest-date (getf db-data :latest-date))
          (is-logged-in (getf db-data :is-logged-in))
          (utc-current (- universal-time (* 9 60 60))))
      (if (and (= is-logged-in 1) (> (abs (- utc-current latest-date)) (* 1 60 60)))
          'time-over
          nil))))

(defun check-login-possible (board-name user-name &optional (hash-string ""))
  (let ((data (get-user-table board-name user-name))
        (session-is-login (gethash *session-login-key* *session*)))
    (unless data
      (return-from check-login-possible nil))
    (when session-is-login
      (return-from check-login-possible 'logged-in))
    (let ((db-hash-string (getf data :hash)))
      (string= hash-string db-hash-string))))

(defun login (board-name user-name password universal-time)
  (when (or (null user-name) (null password))
    (return-from login nil))
  (let* ((hash (sha256 (concatenate 'string *solt* password)))
         (is-login nil)
         (checked (check-login-possible board-name user-name hash))
         (date (get-current-datetime universal-time)))
    (cond ((eq checked 'logged-in)
           'logged-in)
          ((eql checked t)
           (let ((db-data (get-user-table board-name user-name))
                 (is-admin (getf db-data :is-admin))
                 (cap-text (getf db-data :cap-text)))
             (when is-admin
               (setf (gethash *session-admin-key* *session*) t))
             (when (and (not (null cap-text)) (string/= cap-text ""))
               (setf (gethash *session-cap-text-key* *session*) cap-text)))
           (setf (gethash *session-login-key* *session*) t)
           (update-user-table board-name user-name date)
           t)
          (t
           nil))))

(defun create-user (board-name user-name password date &optional (is-admin nil) (cap-text nil))
  (unless (or user-name password)
    (return-from create-user nil))
  (when (get-user-table board-name user-name)
    (return-from create-user 'exist-user))
  (let* ((hash (sha256 (concatenate 'string *solt* password)))
         (date (get-current-datetime date))
         (return-status t)
         (user-data (make-user-table-struct
                     :user-name user-name
                     :hash hash
                     :board-name board-name
                     :create-date date
                     :latest-date date
                     :is-admin (if is-admin 1 0)
                     :cap-text (if cap-text cap-text ""))))
    (handler-case (insert-user-table user-data)
      (error (e)
        (format t "~%Error: ~A~%" e)
        (setq return-status 'create-failed)))
    return-status))


(defun get-dat-data (dat-name)
  (with-open-file (stream dat-name
                          :direction :input
                          :if-does-not-exist nil
                          :external-format :sjis)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun save-html (unixtime html)
  (with-open-file (stream (concatenate 'string *kakolog-html-path* unixtime ".html")
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede)
    (write-line html stream)))

;; WIP implement, convert dat to html when reach max number of save thread in db.
(defun to-kakolog (unixtime dat-file-path)
  (let ((html (dat-to-html dat-file-path)))
    (save-html unixtime html)))

