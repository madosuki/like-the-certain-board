(in-package :cl-user)
(defpackage like-certain-board.web
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
  (:export :*web*))
(in-package :like-certain-board.web)

(deftype mysql-true-type (n) `(= n 1))
(deftype mysql-false-type (n) `(= n 0))
(defvar *mysql-true* 1)
(defvar *mysql-false* 0)

(defstruct user-table-struct
  (board-name "" :type string)
  (user-name "" :type string)
  (hash "" :type string)
  (create-date "" :type string)
  (latest-date "" :type string)
  (ipaddr "" :type string)
  (is-admin nil :type integer)
  (cap-text "" :type string))

(defstruct thread-table-struct
  (title "" :type string)
  (create-date "" :type string)
  (last-modified-date "" :type string)
  (res-count 1 :type integer)
  (unixtime 0 :type intger)
  (ipaddr "" :type string)
  (max 1000 :type integer))

;; this solt is sample. don't use production.
;; (defvar *solt* "wqk0SZoDaZioQbuYzCM3mRBDFbj8FD9sx3ZX34wwhnMjtdAIM2tqonirJ7o8NuDpPkFIFbAacZYTsBRHzjmagGpZZb6aAZVvk5AcWJXWGRdTZlpo7vuXF3zvg1xp9yp0")

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


;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)


;; Functions

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

(defun get-posted-ipaddr-values (table-name ipaddr)
  (with-connection (db)
    (retrieve-one
     (select (fields :count :appearance_date :is_penalty :wait_time)
             (from (intern table-name))
             (where (:like :ipaddr ipaddr))))))

(defun set-posted-ipaddr-count-from-db (table-name ipaddr universal-time n
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
               (where (:like :ipaddr ipaddr)))))))

(defun insert-posted-ipaddr-from-db (table-name ipaddr universal-time)
  (let ((date (get-current-datetime universal-time)))
    (with-connection (db)
      (execute
       (insert-into (intern table-name)
                    (set= :ipaddr ipaddr
                          :appearance_date date
                          :is_penalty 0
                          :count 1
                          :wait_time *default-penalty-time*))))))

(defun get-detail-time-from-universal-time (time &optional (is-utc t))
  (multiple-value-bind (second minute hour date month year day summer timezone)
      (if is-utc (decode-universal-time time) (decode-universal-time time 9))
    (declare (ignore day summer timezone))
    (list :year year :month month :date date :hour hour :minute minute :second second)))

(defmacro hour-to-second (h)
  `(* ,h 60 60))

(defun check-abuse-post (ipaddr time)
  (let* ((table-name "posted_ipaddr_table")
         (fetch-result (get-posted-ipaddr-values table-name ipaddr))
         (current-detail-date (get-detail-time-from-universal-time time))
         (current-second (getf current-detail-date :second))
         (current-minute (getf current-detail-date :minute))
         (current-hour (getf current-detail-date :hour))
         (current-date (getf current-detail-date :date)))
    (unless fetch-result
      (insert-posted-ipaddr-from-db table-name ipaddr time)
      (return-from check-abuse-post t))
    (let* ((appearance-date (getf fetch-result :appearance-date))
           (is-penalty (getf fetch-result :is-penalty))
           (count (getf fetch-result :count))
           (appearance-detail-date (get-detail-time-from-universal-time appearance-date))
           (target-second (getf appearance-detail-date :second))
           (target-minute (getf appearance-detail-date :minute))
           (target-hour (getf appearance-detail-date :hour))
           (target-date (getf appearance-detail-date :date))
           (wait-time (getf fetch-result :wait-time))
           (left  (+ current-second (hour-to-second current-hour) (* current-minute 60)))
           (right (+ target-second (hour-to-second target-hour) (* target-minute 60)))
           (diff (abs (- (if (< target-date current-date) (+ left *24-hour-seconds*) left) right))))
      (when (>= wait-time *24-hour-seconds*)
        (return-from check-abuse-post nil))
      (cond ((<= wait-time diff)
             (setq is-penalty nil)
             (set-posted-ipaddr-count-from-db table-name ipaddr time 0)
             t)
            ((> count 5)
             (set-posted-ipaddr-count-from-db
              table-name ipaddr time (1+ count) t
              (if (< wait-time *24-hour-seconds*) (+ wait-time *default-penalty-time*) wait-time))
             nil)
            (t
             (set-posted-ipaddr-count-from-db table-name ipaddr time (1+ count) nil wait-time)
             nil)))))

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
                    (ipaddr :type 'text
                            :not-null t)
                    (max-line :type 'integer
                              :default *default-max-length*
                              :not-null t)
                    (is-deleted :type 'integer
                                :default *mysql-false*))))))

;; (defvar *user-login-table* "user_login_table")
;; (defun create-user-login-table ()
;;   (with-connection (db)
;;     (execute
;;      (create-table (list (create-user-login-table-name user-name) :if-exists-not t)
;;                    ((id :type 'integer
;;                         :primary-key t
;;                         :auto-increment t)
;;                     (user-name :type 'text
;;                                :not-null t)
;;                     (login-date :type 'datetime
;;                                 :not-null t)
;;                     (ip-address :type '(:varchar 43)
;;                                 :not-null t)
;;                     (session :type 'text))))))

;; (defun insert-user-login-table (user-name ipaddr date session)
;;   (with-connection (db)
;;     (execute
;;      (insert-into :user-login-table
;;                   (set=
;;                    :user-name user-name
;;                    :login-date date
;;                    :ip-address ipaddr
;;                    :session session)))))

;; (defun get-latest-login-data-from-user-login-table (user-name)
;;   (with-connection (db)
;;     (retrieve-one
;;      (select :*
;;              (from :user-login-table)
;;              (where (:= :user-name user-name))
;;              (order-by (:desc :login-date))))))

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
        (ipaddr (user-table-struct-ipaddr user-data))
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
                     :ipaddr ipaddr
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
  (let* ((datetime (replace-hyphen-to-slash (get-current-datetime date t -9)))
         (id (generate-id :ipaddr ipaddr :date datetime :solt *solt*))
         (trip (if (and (stringp trip-key) (string/= trip-key ""))
                   (generate-trip (subseq trip-key 1 (length trip-key)) "utf8")
                   ""))
         (final-text
           (apply-dice
            (create-safety-strings
             (shape-text
              (replace-other-line-to-lf text)))))
         (mail (create-safety-strings (convert-html-special-chars email)))
         (final-name (create-safety-strings (convert-html-special-chars name))))
    (when (string/= trip "")
      (setq trip (concatenate 'string "</b>" (string #\BLACK_DIAMOND) trip "<b>")))
    (if first
        (format nil "~A~A<>~A<>~A ID:~A<>~A<>~A~%" (apply-dice final-name t) trip mail datetime id final-text title)
        (format nil "~A~A<>~A<>~A ID:~A<>~A<>~%" (apply-dice final-name t) trip mail datetime id final-text))))

(defun create-thread-in-db (&key title create-date unixtime ipaddr max-line)
  (let ((date (get-current-datetime create-date)))
    (with-connection (db)
      (execute
       (insert-into :threads
                    (set= :title title
                          :create-date date
                          :last-modified-date date
                          :res-count 1
                          :unixtime unixtime
                          :ipaddr ipaddr
                          :max-line (if (or (null max-line) (< max-line *default-max-length*))
                                        *default-max-length*
                                        max-line)))))))

(defun change-max-of-thread-in-db (unixtime)
  (with-connection (db)
    (execute
     (update :thread
             (set= :max 10000)
             (where (:like :unixtime unixtime))))))

(defun decode-max-line-string (s)
  (let ((regex-str "!max_line=[\\d+]$"))
    (regex-group-bind (number)
                      (regex-str s)
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
  (let* ((title (get-value-from-key-on-list "subject" _parsed))
         (name (get-value-from-key-on-list "FROM" _parsed))
         (trip-key "")
         (text (get-value-from-key-on-list "MESSAGE" _parsed))
         (email (get-value-from-key-on-list "mail" _parsed))
         (max-line (escape-string (get-value-from-key-on-list "max_line" _parsed)))
         (unixtime (get-unix-time date))
         (is-cap nil))
    (when (and (gethash *session-login-key* *session*) (gethash *session-cap-text-key* *session*))
      (setq is-cap t))
    (when (eq email 'no-data)
      (setq email ""))
    (when (eq text 'no-data)
      (setq text ""))
    (when (eq name 'no-data)
      (setq name *default-name*))
    ;; (setq email (replace-not-available-char-when-cp932 email))
    ;; (setq name (replace-not-available-char-when-cp932 email))
    (if (stringp title)
        (progn
          (setq title (create-safety-strings title))
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
                                                                   :ipaddr ipaddr
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
    (when (eq from 'no-data)
      (setq from *default-name*))
    (when (eq mail 'no-data)
      (setq mail ""))
    (when (or (eq bbs 'no-data)
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

(defun put-thread-list (board-name)
  (if (string= board-name *board-name*)
      (progn
        (check-exists-threads-table-and-create-table-when-does-not)
        (let ((result (get-thread-list))
              (is-login (gethash *session-login-key* *session*)))
          (dolist (x result)
            (setf (getf x :create-date) (format-datetime (getf x :create-date)))
            (setf (getf x :last-modified-date) (format-datetime (getf x :last-modified-date))))
          (render #P"board.html" (list :board-name *board-title*
                                       :bbs board-name
                                       :time (get-unix-time (get-universal-time))
                                       :threads result
                                       :is-login is-login))))
      (on-exception *web* 404)))

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
  (let* ((decoded-query (sb-ext:octets-to-string (coerce body-text '(vector (unsigned-byte 8)))
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
               (when (> (cadr (get-res-count :key key)) *default-max-length*)
                 503)
               (let ((status (insert-res form ipaddr universal-time)))
                 (if (= status 200)
                     (progn
                       (setf (getf (response-headers *response*) :location) (concatenate 'string "/test/read.cgi/" bbs "/" key))
                       (set-response-status 302))
                     (progn
                       ;; (setf (response-status *response*) status)
                       (set-response-status status)
                       )))
               (next-route))
              ((string= submit "新規スレッド作成")
               (let ((status (create-thread :_parsed form :date universal-time :ipaddr ipaddr)))
                 (if (= status 200)
                     (progn (setf (getf (response-headers *response*) :location) (concatenate 'string "/" bbs))
                            (set-response-status 302))
                     (set-response-status status)
                     ))
               (next-route))
              (t
               (set-response-status 400)
               (next-route)))))
        (progn
          (set-response-status 400)
          (next-route)))))

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

(defun login (board-name user-name password ipaddr universal-time)
  (when (or (null user-name) (null password))
    (return-from login nil))
  (let* ((hash (sha256 (concatenate 'string *solt* password)))
         (is-login nil)
         (checked (check-login-possible board-name user-name hash))
         (date (get-current-datetime universal-time)))
    (cond ((eq checked 'logged-in)
           'logged-in)
          ((eql checked t)
           (let* ((db-data (get-user-table board-name user-name))
                  (is-admin (getf db-data :is-admin))
                  (cap-text (getf db-data :cap-text)))
             (when is-admin (setf (gethash *session-admin-key* *session*) t))
             (when (and (not (null cap-text)) (string/= cap-text ""))
               (setf (gethash *session-cap-text-key* *session*) cap-text)))
           (setf (gethash *session-login-key* *session*) t)
           (update-user-table board-name user-name date)
           t)
          (t
           nil))))

(defun create-user (board-name user-name password ipaddr date &optional (is-admin nil) (cap-text nil))
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
                     :ipaddr ipaddr
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

;; Model
;; (defmodel (threads
;;            (:inflate created-date update-date #'datetime-to-timestamp))
;;   id
;;   title
;;   res-count
;;   created-date
;;   update-date)

;;
;; Routing rules

(defroute "/" ()
  (render #P"index.html"))

(defroute ("/:board-name/" :method :GET) (&key board-name)
  ;; (maphash #'(lambda (key value) (format t "~%Key:~A, Value:~A~%" key value)) (request-headers *request*))
  ;; (print (gethash "cookie" (request-headers *request*)))
  (put-thread-list board-name))

(defroute ("/:board-name" :method :GET) (&key board-name)
  (put-thread-list board-name))

(defroute ("/:board-name/subject.txt" :method :GET) (&key board-name)
  (declare (ignore board-name))
  (let* ((tmp (get-thread-list-when-create-subject-txt))
         (result nil))
    (if tmp
        (progn
          (dolist (x tmp)
            (let ((dat-name (generate-dat-name x))
                  (title (cadr (member :title x)))
                  (res-count (cadr (member :res-count x))))
              (push (format nil "~A<>~A (~A)~%" dat-name title res-count) result)))
          (let* ((final (apply #'concatenate 'string (nreverse result)))
                 (oct (sb-ext:string-to-octets final :external-format :sjis))
                 (content-length (length oct)))
            `(200 (:content-type "text/plain" :content-length ,content-length) ,oct)))
        (let* ((empty (sb-ext:string-to-octets "" :external-format :sjis))
               (content-length (length empty)))
          `(200 (:content-type "text/plain" :content-length ,content-length) ,empty)))))

(defroute "/ipinfo" ()
  (caveman2:request-remote-addr caveman2:*request*))

(defroute ("/test/read.cgi/:board-name/:unixtime" :method :GET) (&key board-name unixtime)
  (declare (ignore board-name))
  (let* ((filepath (concatenate 'string *dat-path* unixtime ".dat"))
         (dat-list (dat-to-keyword-list filepath))
         (title (cadr (member :title (car dat-list))))
         (current-unix-time (get-unix-time (get-universal-time)))
         (is-login (gethash *session-login-key* *session*)))
    (if (probe-file filepath)
        (render #P "thread.html" (list :title title :thread dat-list :bbs *board-name* :key unixtime :time current-unix-time :is-login is-login))
        (on-exception *web* 404))))


(defroute ("/test/bbs.cgi" :method :POST) (&key _parsed)
  (let ((ipaddr (caveman2:request-remote-addr caveman2:*request*))
        (universal-time (get-universal-time)))
    (if (check-abuse-post ipaddr universal-time)
        (let* ((message (get-value-from-key "MESSAGE" _parsed))
               (cookie (gethash "cookie" (request-headers *request*)))
               (splited-cookie (if (null cookie)
                                   nil
                                   (mapcar #'(lambda (v) (cl-ppcre:split "=" v))
                                           (cl-ppcre:split ";" cookie))))
               (raw-body (request-raw-body *request*))
               (content-length (request-content-length *request*))
               (tmp-array (make-array content-length :adjustable t :fill-pointer content-length)))
          (read-sequence tmp-array raw-body)
          (bbs-cgi-function tmp-array ipaddr universal-time))
        (let ((bbs (cdr (assoc "bbs" _parsed :test #'string=)))
              (key (cdr (assoc "key" _parsed :test #'string=))))
          ;; (setf (response-status *response*) 429)
          (set-response-status 429)
          (render #P "time_restrict.html" (list
                                           :ipaddr ipaddr
                                           :minute
                                           (/ (getf (get-posted-ipaddr-values "posted_ipaddr_table" ipaddr)
                                                    :wait-time)
                                              60)
                                                     
                                           :bbs bbs
                                           :key (if key key nil)))))))


(defroute ("/:board-name/dat/:unixtime.dat" :method :GET) (&key board-name unixtime)
  (declare (ignore board-name))
  (let ((pathname (probe-file (concatenate 'string *dat-path* unixtime ".dat"))))
    (if (not (null pathname))
        (progn
          (setf (getf (response-headers *response*) :content-type) "text/plain; charset=Shift_jis")
          (setf (response-body *response*) pathname))
        (progn
          ;; (setf (response-status *response*) 404)
          (set-response-status 404)
          ""))))


(defroute ("/:board-name/SETTING.TXT" :method :GET) (&key board-name)
  (if (string= board-name *board-name*)
      (let ((pathname "SETTING.txt"))
        (setf (getf (response-headers *response*) :content-type) "text/plain; charset=Shift_jis")
        (setf (response-body *response*) (probe-file pathname)))
      (on-exception *web* 404)))

(defroute ("/:board-name/login" :method :GET) (&key board-name)
  (render #P "login.html" (list :bbs board-name
                                :is-login (if (gethash *session-login-key* *session*)
                                              "logged-in"
                                              ""))))

(defroute ("/:board-name/api/user" :method :POST) (&key board-name _parsed)
  (let* ((ipaddr (caveman2:request-remote-addr caveman2:*request*))
         (is-login (get-value-from-key "is_login" _parsed))
         (user-name (get-value-from-key "user_name" _parsed))
         (password (get-value-from-key "password" _parsed))
         (is-admin (get-value-from-key "is_admin" _parsed))
         (cap-text (get-value-from-key "cap_text" _parsed))
         (cookie (gethash "cookie" (request-headers *request*)))
         (splited-cookie (if (null cookie)
                             nil
                             (mapcar #'(lambda (v) (cl-ppcre:split "=" v))
                                     (cl-ppcre:split ";" cookie))))
         (date (get-universal-time)))
    (if (or (null user-name)
            (null password))
        (progn
          (set-response-status 401)
          (next-route))
        (progn
          (setq user-name (create-safety-strings user-name))
          (if is-login
              (let ((login-check (login board-name user-name password ipaddr date)))
                (cond ((eq login-check 'logged-in)
                       (render #P "login.html" (list :bbs board-name :is-login "logged-in")))
                      ((eql login-check t)
                       (setf (getf (response-headers *response*) :location) (concatenate 'string "/" board-name))
                       (set-response-status 302)
                       (next-route))
                      (t
                       (set-response-status 401)
                       (render #P "login.html" (list :bbs board-name :is-login "failed")))))
              (if (string= ipaddr *admin-ipaddr*)
                  (let ((create-result (create-user board-name user-name password ipaddr date is-admin cap-text)))
                    (set-response-status 401)
                    (cond ((eq create-result 'exist-user)
                           "This User Name is existed.")
                          ((eq create-result 'create-failed)
                           "Error, failed create account.")
                          ((eql create-result t)
                           (set-response-status 200)
                           "Finish Create that Account.")
                          (t
                           "that is invalid parameter.")))
                  (progn
                    (set-response-status 401)
                    "This ip aadress is don't trust.")))))))


(defroute ("/:board-name/api/line" :method :POST) (&key board-name _parsed)
  (let* ((key (get-value-from-key "key" _parsed))
         (line (get-value-from-key "line" _parsed))
         (line-number (parse-integer (if (stringp line) line "") :junk-allowed t))
         (is-login (gethash *session-login-key* *session*))
         (is-admin (gethash *session-admin-key* *session*)))
    (cond ((null is-login)
           ;; (setf (response-status *response*) 403)
           (set-response-status 403)
           "not was logged-in")
          ((null is-admin)
           ;; (setf (response-status *response*) 403)
           (set-response-status 403)
           "you are not admin")
          ((or (null line) (null key))
           ;; (setf (response-status *response*) 400)
           (set-response-status 400)
           "invalid param")
          ((and (numberp line-number) (string= board-name *board-name*))
           (if (delete-line-in-dat key line-number)
                 (let* ((filepath (concatenate 'string *dat-path* key ".dat"))
                        (dat-list (dat-to-keyword-list filepath))
                        (title (cadr (member :title (car dat-list))))
                        (current-unix-time (get-unix-time (get-universal-time)))
                        (is-login (gethash *session-login-key* *session*)))
                   (render #P "thread.html" (list :title title :thread dat-list :bbs board-name :key key :time current-unix-time :is-login is-login)))
                 "faild delete."))
          (t
           (on-exception *web* 404)))))

(defroute ("/:board-name/api/thread" :method :POST) (&key board-name _parsed)
  (let ((key (get-value-from-key "key" _parsed))
        (is-login (gethash *session-login-key* *session*))
        (is-admin (gethash *session-admin-key* *session*)))
    (cond ((or (null mode) (null is-login) (null is-admin))
           (set-response-status 403)
           "invalid params")
          ((string= mode "delete")
           (if (and (not (null key)) (check-exist-row (parse-integer key)))
               (let ((filepath (concatenate 'string *dat-path* key ".dat")))
                 (when (probe-file filepath)
                   (delete-thread (parse-integer key))
                   (delete-file filepath)
                   (setf (getf (response-headers *response*) :location) (concatenate 'string "/" board-name))
                   (set-response-status 302)
                   (next-route)))
               (prgon
                (set-response-status 403)
                "invalid params")))
          ((string= mode "log")
           (if (and (not (null key)) (check-exist-row (parse-integer key)))
               (let ((filepath (concatenate 'string *dat-path* key ".dat")))
                 (when (probe-file filepath)
                   (to-kakolog key filepath)
                   (delete-thread (parse-integer key))
                   (delete-file filepath)
                   (setf (getf (response-headers *response*) :location) (concatenate 'string "/" board-name))
                   (set-response-status 302)
                   (next-route)))
               (prgon
                (set-response-status 403)
                "invalid params")))
          (t
           (set-response-status 410)
           "not exists this thread."))))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
