(in-package :cl-user)
(defpackage like-certain-board.webfunctions
  (:use :cl
        :caveman2
        :like-certain-board.config
        :like-certain-board.view
        :like-certain-board.db
        :quri
        :cl-fad
   :generate-like-certain-board-strings)
  (:import-from :like-certain-board.utils
                :write-log
                :separate-numbers-from-key-for-kako
                :check-whether-integer)
  (:export
   :put-thread-list
   :to-kakolog
   :bbs-cgi-function
   :get-value-from-key
   :check-abuse-post
   :set-response-status
   :login
   :create-user
   :generate-dat-name
   :convert-bunch-of-thread-to-kakolog
   :kakolog-process
   :delete-line-in-dat))
(in-package :like-certain-board.webfunctions)

(deftype mysql-true-type (n) `(= n 1))
(deftype mysql-false-type (n) `(= n 0))
(defvar *mysql-true* 1)
(defvar *mysql-false* 0)


(defmacro caddddr (v)
  `(caddr (cddr ,v)))

(defmacro cadddddr (v)
  `(caddr (cdddr ,v)))

(defmacro regex-group-bind (var-list regex-string target-string &body body)
  `(cl-ppcre:register-groups-bind (,@var-list)
                                  (,regex-string ,target-string)
                                  ,@body))
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
        (write-log :mode :error
                   :message (format nil "not reach latest length cookie"))
        (return-from get-session-from-cookie nil))
      (let ((session (member "app.session" splited-cookie :test #'equal)))
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

(defun delete-line-in-dat (key bbs line-number)
  (let* ((path (format nil "~A/~A/~A.dat" *dat-path* bbs key))
         (outpath (concatenate 'string path ".out"))
         (is-not-error nil))
    (handler-case (change-line-in-dat path outpath *delete-message* line-number)
      (error (e)
        (write-log :mode :error
                   :message e))
      (:no-error (c)
        (declare (ignore c))
        (rename-file outpath path)
        (setq is-not-error t)))
    is-not-error))




(defun get-detail-time-from-universal-time (time &optional (is-utc t))
  (multiple-value-bind (second minute hour date month year day summer timezone)
      (if is-utc (decode-universal-time time) (decode-universal-time time 9))
    (declare (ignore day summer timezone))
    (list :year year :month month :date date :hour hour :minute minute :second second)))

(defmacro hour-to-second (h)
  `(* ,h 60 60))

(defun check-abuse-post (&key current-unixtime user-agent ipaddr session)
  (unless user-agent
    (return-from check-abuse-post :restrict))
  (let* ((data (get-data-from-time-restrict :ipaddr ipaddr)))
    (unless data
      (insert-to-time-restrict-table :ipaddr ipaddr
                                     :last-unixtime current-unixtime)
      (return-from check-abuse-post :ok))
    (let* ((count (cadr (member :count data)))
           (last-unixtime (cadr (member :last-unixtime data)))
           (penalty-count (cadr (member :penalty-count data)))
           (diff (- current-unixtime last-unixtime)))
      (cond ((> penalty-count 10)
             :ban)
            ((and (> diff 10) (< count 100))
             (update-time-restrict-count-and-last-unixtime :ipaddr ipaddr
                                                           :count 0
                                                           :penalty-count penalty-count
                                                           :last-unixtime current-unixtime)
             :ok)
            ((and (>= count 100) (> diff *24-hour-seconds*))
             (update-time-restrict-count-and-last-unixtime :ipaddr ipaddr
                                                           :count 0
                                                           :penalty-count (1+ penalty-count)
                                                           :last-unixtime current-unixtime)
             :ok)
            ((>= count 100)
             :restrict-24)
            ((< diff 10)
             (update-time-restrict-count-and-last-unixtime :ipaddr ipaddr
                                                           :count (1+ count)
                                                           :penalty-count penalty-count
                                                           :last-unixtime current-unixtime)
             :restrict)))))


(defun format-datetime (date)
  (multiple-value-bind (second minute hour date month year day summer timezone)
      (decode-universal-time date)
    (declare (ignore day summer timezone))
    (format nil "~A/~2,'0d/~2,'0d ~2,'0d:~2,'0d:~2,'0d" year month date hour minute second)))



(defun check-exists-threads-table ()
  (handler-case (check-exists-table "threads")
    (error (e)
      (declare (ignore e))
      :not-exists)))

(defmacro get-value-from-key (key target)
  `(cdr (assoc ,key ,target :test #'string=)))

(defmacro get-value-from-key-on-list (key target)
  `(cadr (member ,key ,target :test #'string=)))

(defmacro generate-dat-name (l)
  `(concatenate 'string (write-to-string (cadr (member :unixtime ,l))) ".dat"))


(defun create-dat (&key unixtime first-line board-url-name)
  (let* ((dat-dir (format nil "~A/~A" *dat-path* board-url-name))
         (filename (concatenate 'string (write-to-string unixtime) ".dat"))
         (path (format nil "~A/~A" dat-dir filename)))
    (unless (cl-fad:directory-exists-p *dat-path*)
      (ensure-directories-exist *dat-path*))
    (unless (cl-fad:directory-exists-p dat-dir)
      (ensure-directories-exist dat-dir))
    (with-open-file (i path
                       :direction :output
                       :if-does-not-exist :create
                       :element-type '(unsigned-byte 8))
      (let ((tmp (sb-ext:string-to-octets first-line :external-format :sjis)))
        (write-sequence tmp i)))))

(defun create-res (&key name trip-key email date text ipaddr (first nil) (title ""))
  (format t "~%trip key: ~A~%" trip-key)
  (let* ((datetime (replace-hyphen-to-slash date))
         (id (generate-id :ipaddr ipaddr :date datetime :salt *salt*))
         (trip (if (and (stringp trip-key) (string/= trip-key ""))
                   (generate-trip (subseq trip-key 1) "utf8")
                   ""))
         (final-text (replace-not-available-char-when-cp932 (shape-text (replace-other-line-to-lf text)))) ;; did convert-html-special-chars in shape-text function
         (mail (replace-not-available-char-when-cp932 (convert-html-special-chars email)))
         (final-name (apply-dice (replace-not-available-char-when-cp932 (convert-html-special-chars name))
                                 t)))
    (when (string/= trip "")
      (setq trip (concatenate 'string "</b>" (string #\BLACK_DIAMOND) trip "<b>")))
    (if first
        (format nil "~A~A<>~A<>~A ID:~A<>~A<>~A~%" final-name trip mail datetime id final-text title)
        (format nil "~A~A<>~A<>~A ID:~A<>~A<>~%" final-name trip mail datetime id final-text))))


(defun decode-max-line-string (target)
  (let ((regex-str "max_line=(?!0)([0-9]+)"))
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


(defun create-thread (&key _parsed date ipaddr board-id)
  (when (eq (check-exists-threads-table) :no-exists)
    (return-from create-thread 400))
  (let ((title (get-value-from-key-on-list "subject" _parsed))
        (name (get-value-from-key-on-list "FROM" _parsed))
        (trip-key "")
        (text (get-value-from-key-on-list "MESSAGE" _parsed))
        (email (get-value-from-key-on-list "mail" _parsed))
        (max-line (get-value-from-key-on-list "max_line" _parsed))
        (unixtime (get-unix-time date))
        (bbs (get-value-from-key-on-list "bbs" _parsed))
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
      (setq name (cadr (get-default-name-from-id board-id))))
    (if (stringp title)
        (progn
          (setq title (replace-not-available-char-when-cp932 (convert-html-special-chars title)))
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
          (labels ((progress (title date unixtime ipaddr name text board-id &optional (count 0))
                     (let ((is-error nil)
                           (formatted-date (get-current-datetime date)))
                       (handler-case (create-thread-in-db :title title
                                                          :date formatted-date
                                                          :unixtime unixtime
                                                          :max-line max-line
                                                          :board-id board-id)
                         (error (e)
                           (write-log :mode :error
                                      :message (format nil "Error in create-thread-function: ~A" e))
                           (setq is-error t)))
                       (if is-error
                           (progn
                             (incf unixtime)
                             (if (< count 10)
                                 (progress title date unixtime ipaddr name text board-id (incf count))
                                 (return-from create-thread 400)))
                           (progn (handler-case  (create-dat
                                                  :board-url-name bbs
                                                  :unixtime unixtime
                                                  :first-line (create-res :name (if is-cap
                                                                                    (gethash *session-cap-text-key* *session*)
                                                                                    name)
                                                                          :trip-key trip-key
                                                                          :email email
                                                                          :text text
                                                                          :ipaddr ipaddr
                                                                          :date formatted-date
                                                                          :first t
                                                                          :title title))
                                    (error (e)
                                      (write-log :mode :error
                                                 :message (format nil "Error failed create-dat function: ~A" e))
                                      (delete-thread unixtime board-id)
                                      (incf unixtime)
                                      (if (< count 10)
                                          (progress title date unixtime ipaddr name text board-id (incf count))
                                          (return-from create-thread 400))))
                                  200)))))
            (progress title date unixtime ipaddr name text board-id)))
        400)))

(defun insert-res (_parsed ipaddr universal-time board-id)
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
      (setq from (cadr (get-default-name-from-id board-id))))
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
      (setq from (cadr (get-default-name-from-id board-id))))
    (when (and (string/= time "") (< (cadr (get-res-count :key key)) (cadr (get-max-line :key key)))
               (> (get-unix-time universal-time) (parse-integer time :radix 10)))
      (with-open-file (input (format nil "~A/~A/~A.dat" *dat-path* bbs key)
                             :direction :output
                             :if-exists :append
                             :if-does-not-exist nil
                             :element-type '(unsigned-byte 8)
                             :external-format :sjis)
        (let* ((tmp (separate-trip-from-input from))
               (name (car tmp))
               (trip (if (> (length tmp) 1)
                         (cadr tmp)
                         ""))
               (formatted-date (get-current-datetime universal-time)))
          (let ((res (create-res
                      :name (if is-cap (gethash *session-cap-text-key* *session*) name)
                      :trip-key trip
                      :email mail
                      :text message
                      :ipaddr ipaddr
                      :date formatted-date)))
            (write-sequence (sb-ext:string-to-octets res :external-format :sjis) input)
            (if (equal mail "sage")
                (update-last-modified-date-of-thread :date formatted-date :key key :board-id board-id)
                (update-last-dates-of-thread :date formatted-date :key key :board-id board-id))
            (update-res-count-of-thread :key key :board-id board-id)
            (when (>= (cadr (get-res-count :key key)) (cadr (get-max-line :key key)))
              (write-sequence
               (sb-ext:string-to-octets *1001* :external-format :sjis) input)
              (update-res-count-of-thread :key key :board-id board-id)
              (if (equal mail "sage")
                  (update-last-modified-date-of-thread :date formatted-date :key key :board-id board-id)
                  (update-last-dates-of-thread :date formatted-date :key key :board-id board-id)))
            (write-log :mode :changes-result
                       :message (format nil "insert: ~A" time))))))
    status))

(defun put-thread-list (board-name board-title web url csrf-token)
  (let ((board-list-data (get-a-board-name-from-name board-name)))
    (if board-list-data
        (progn
          (when (eq (check-exists-threads-table) :not-exists)
            (return-from put-thread-list 400))
          (let ((result (get-thread-list (getf board-list-data :id)))
                (is-login (gethash *session-login-key* *session*)))
            (dolist (x result)
              (setf (getf x :create-date) (format-datetime (getf x :create-date)))
              (setf (getf x :last-modified-date) (format-datetime (getf x :last-modified-date))))
            (board-view :board-name board-title
                        :bbs board-name
                        :time (get-unix-time (get-universal-time))
                        :csrf-token csrf-token
                        :thread-list result
                        :is-login is-login
                        :url url)))
        (on-exception web 404))))

(defun get-param (body)
  (let ((tmp (cl-ppcre:split "&" body))
        (result nil))
    (dolist (x tmp)
      (push (cl-ppcre:split "=" x) result))
    (nreverse result)))

(defun try-url-decode (x &optional (encode :UTF-8) (error-count 0))
  ;; (format t "~%url-encode: ~A~%" x)
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
          (let* ((bbs (get-value-from-key-on-list "bbs" form))
                 (board-id (let ((board-list-data (get-a-board-name-from-name bbs)))
                             (if board-list-data
                                 (getf board-list-data :id)
                                 nil)))
                 (key (get-value-from-key-on-list "key" form))
                 (submit (get-value-from-key-on-list "submit" form))
                 (check-key (check-whether-integer key)))
            (cond
              ((string= submit "書き込む")
               (cond ((or (null (eq check-key :integer-string)) (null board-id))
                      (set-response-status 400)
                      (write-result-view :error-type 'unknown :message "bad parameter"))
                     ((> (cadr (get-res-count :key key)) *default-max-length*)
                      (set-response-status 503)
                      (write-result-view :error-type 'write-error :message "このスレッドはレス数が最大数に達しています．"))
                     (t
                      (let* ((status (insert-res form ipaddr universal-time board-id)))
                        (if (= status 200)
                            (progn
                              (setf (getf (response-headers *response*) :location) (concatenate 'string "/test/read.cgi/" bbs "/" key))
                              (set-response-status 302)
                              "書き込みに成功しました．遷移します．")
                            (progn
                              (set-response-status status)
                              (write-result-view :error-type 'write-error :message "混雑等の理由で新規スレッド作成に失敗しました．")))))))
              ((string= submit "新規スレッド作成")
               (if (null board-id)
                   (progn (set-response-status 400)
                          (write-result-view :error-type 'create-error :message "bad parameter: require bbs param."))
                   (let ((status (create-thread
                                  :_parsed form
                                  :date universal-time
                                  :ipaddr ipaddr
                                  :board-id board-id)))
                     (if (= status 200)
                         (progn (setf (getf (response-headers *response*) :location) (concatenate 'string "/" bbs))
                                (set-response-status 302)
                                "スレッド作成に成功しました．遷移します．")
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


(defun check-time-over-logged-in (board-id user-name universal-time)
  (let ((db-data (get-user-table board-id user-name)))
    (unless db-data
      (return-from check-time-over-logged-in :not-found))
    (let ((latest-date (getf db-data :latest-date))
          (is-logged-in (getf db-data :is-logged-in)))
      (if (and (= is-logged-in 1)
               (> (abs (- universal-time latest-date)) (* 1 60 60)))
          :time-over
          nil))))

(defun check-login-possible (board-id user-name &optional (hash-string ""))
  (let ((data (get-user-table board-id user-name))
        (is-logged-in (gethash *session-login-key* *session*)))
    (unless data
      (return-from check-login-possible (cons :not-found :no-data)))
    (when is-logged-in
      (return-from check-login-possible (cons :logged-in data)))
    (let ((db-hash-string (getf data :hash)))
      (if (string= hash-string db-hash-string)
          (cons :success data)
          (cons :failed :no-data)))))

(defun login (board-id user-name password universal-time)
  (when (or (null user-name) (null password))
    (return-from login nil))
  (let* ((hash (sha256 (concatenate 'string *salt* password)))
         (is-login nil)
         (checked-v (check-login-possible board-id user-name hash))
         (date (get-current-datetime universal-time)))
    (cond ((eq (car checked-v) :logged-in)
           :logged-in)
          ((eq (car checked-v) :success)
           (let* ((db-data (cdr checked-v))
                  (is-admin (getf db-data :is-admin))
                  (cap-text (getf db-data :cap-text)))
             (when is-admin
               (setf (gethash *session-admin-key* *session*) t))
             (when (and (not (null cap-text)) (string/= cap-text ""))
               (setf (gethash *session-cap-text-key* *session*) cap-text)))
           (setf (gethash *session-login-key* *session*) t)
           (update-user-table board-name user-name date)
           :success)
          (t
           :failed))))

(defun create-user (board-id user-name password date &optional (is-admin nil) (cap-text nil))
  (unless (or user-name password)
    (return-from create-user nil))
  (when (get-user-table board-id user-name)
    (return-from create-user :exist-user))
  (let* ((hash (sha256 (concatenate 'string *salt* password)))
         (date (get-current-datetime date))
         (return-status t)
         (user-data (make-user-table-struct
                     :user-name user-name
                     :hash hash
                     :board-id board-id
                     :create-date date
                     :latest-date date
                     :is-admin (if is-admin 1 0)
                     :cap-text (if cap-text
                                   (convert-html-special-chars (replace-not-available-char-when-cp932 (format nil "~A★" cap-text)))
                                   ""))))
    (handler-case (insert-user-table user-data)
      (error (e)
        (write-log :mode :error
                   :message (format nil "~%Error in create-user : ~A~%" e))
        (setq return-status :create-failed)))
    return-status))


(defun get-dat-data (dat-file-path)
  (with-open-file (stream dat-file-path
                          :direction :input
                          :if-does-not-exist nil
                          :external-format :sjis)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun save-html (&key board-url-name html outpath)
  (handler-case
      (progn (with-open-file (out-s outpath
                                    :direction :output
                                    :if-does-not-exist :create
                                    :if-exists :supersede)
               (write-line html out-s))
             'success)
    (error (e)
      (write-log :mode :error
                 :message (format nil "Error in save-html: ~A" e))
      nil)))

;; WIP implement, convert dat to html when reach max number of save thread in db.
(defun to-kakolog (board-url-name dat-file-path outpath)
  (let ((html (dat-to-html dat-file-path)))
    (if (save-html :board-url-name board-url-name
                   :html html
                   :outpath outpath)
        'success
        nil)))


(defun kakolog-process (&key key title board-url-name board-id)
  (let ((separated (separate-numbers-from-key-for-kako key)))
    (if (or (eq separated :not-numbers) (eq separated :small))
        (progn
          (write-log :mode :error
                     :message separated)
          :failed)
        (let* ((1st (car separated))
               (2nd (cdr separated))
               (orig-dat-filepath (format nil "~A/~A/~A.dat"
                                          *dat-path* board-url-name key))
               (kakolog-dat-dir-root-path (format nil "~A/~A/" *kakolog-dat-path* board-url-name))
               (kakolog-dat-dir-path-with-4digit (format nil "~A~A"
                                                   kakolog-dat-dir-root-path
                                                   1st))
               (kakolog-dat-dir-path (format nil "~A/~A/"
                                             kakolog-dat-dir-path-with-4digit
                                             2nd))
               (kakolog-dat-filepath (format nil "~A~A.dat" kakolog-dat-dir-path key))
               (kakolog-html-dir-root-path (format nil "~A/~A/" *kakolog-html-path* board-url-name))
               (kakolog-html-dir-path-with-4digit (format nil "~A~A/"
                                                          kakolog-html-dir-root-path
                                                          1st))
               (kakolog-html-dir-path (format nil "~A~A/"
                                             kakolog-html-dir-path-with-4digit
                                             2nd))
               (kakolog-html-filepath (format nil "~A~A.html" kakolog-html-dir-path key)))
          (if (probe-file orig-dat-filepath)
              (if (to-kakolog board-url-name orig-dat-filepath kakolog-html-filepath)
                  (let ((c nil))
                    (unless (cl-fad:directory-exists-p kakolog-dat-dir-root-path)
                      (ensure-directories-exist kakolog-dat-dir-root-path))
                    (unless (cl-fad:directory-exists-p kakolog-dat-dir-path-with-4digit)
                      (ensure-directories-exist kakolog-dat-dir-path-with-4digit))
                    (unless (cl-fad:directory-exists-p kakolog-dat-dir-path)
                      (ensure-directories-exist kakolog-dat-dir-path))
                    (unless (cl-fad:directory-exists-p kakolog-html-dir-root-path)
                      (ensure-directories-exist kakolog-html-dir-root-path))
                    (unless (cl-fad:directory-exists-p kakolog-html-dir-path-with-4digit)
                      (ensure-directories-exist kakolog-html-dir-path-with-4digit))
                    (unless (cl-fad:directory-exists-p kakolog-html-dir-path)
                      (ensure-directories-exist kakolog-html-dir-path))
                    (handler-case (copy-file orig-dat-filepath kakolog-dat-filepath)
                      (error (e)
                        (write-log :mode :error
                                   :message (format nil "Error in kakolog-process: ~A" e))
                        (when (probe-file kakolog-dat-filepath)
                          (delete-file kakolog-dat-filepath))
                        (when (probe-file kakolog-html-filepath)
                          (delete-file kakolog-html-filepath))
                        (setq c :failed-copy-from-dat)))
                    (if (null c)
                        (if (probe-file orig-dat-filepath)
                            (progn
                              (delete-file orig-dat-filepath)
                              (delete-thread key board-id)
                              (insert-kakolog-table key title board-id)
                              :success)
                            (progn
                              (when (probe-file kakolog-dat-filepath)
                                (delete-file kakolog-dat-filepath))
                              (when (probe-file kakolog-html-filepath)
                                (delete-file kakolog-html-filepath))
                              :failed-delete-dat))
                        c))
                  (progn
                    (write-log :mode :error
                               :message (format nil "failed convert dat to html: ~A" orig-dat-filepath))
                    :faild-convert-dat-to-html))
              (progn
                (write-log :mode :error
                           :message (format nil "not exists dat file: ~A" orig-dat-filepath))
                :not-exists-that-a-dat-file))))))

(defun convert-bunch-of-thread-to-kakolog (board-id)
  (let ((thread-list (if board-id
                         (get-expired-thread-list (get-current-datetime (get-universal-time))
                                                  board-id)
                         nil))
       (result nil))
    (unless thread-list
      (return-from convert-bunch-of-thread-to-kakolog nil))
    (dolist (x thread-list)
      (let* ((key (getf x :unixtime))
             (title (getf x :title)))
        (push (cons key (kakolog-process :key key :title title :board-url-name board-url-name :board-id board-id)) result)))
    (nreverse result)))

