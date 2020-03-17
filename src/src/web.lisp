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
        :generate-like-certain-board-strings)
  (:export :*web*))
(in-package :like-certain-board.web)

(defvar *default-name* "名無しさん")
(defvar *board-name* "yarufre")
(defvar *default-max-length* 1000)
(defvar *1001* "1001<><>Over Max Thread<> Reached max. Can't write to this thread. <>")
(defvar *default-penalty-time* 60)
(defvar *24-hour-seconds* 86400)
(defvar *9-hour-seconds* (* 9 60 60))

(defmacro caddddr (v)
  `(caddr (cddr ,v)))

(defmacro cadddddr (v)
  `(caddr (cdddr ,v)))


;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

(defmacro escape-string (text)
  `(escape-sql-query (replace-not-available-char-when-cp932 ,text)))

;; Functions 
(defun get-thread-list ()
  (with-connection (db)
    (retrieve-all
     (select :* (from :threads)
             (order-by (:desc :last-modified-date))))))

(defun get-thread-list-when-create-subject-txt ()
  (with-connection (db)
    (retrieve-all
     (select (fields :title :res-count :unixtime)
             (from :threads)
             (order-by (:desc :last-modified-date))))))

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
  (let ((date (get-current-datetime universal-time t 0)))
    (with-connection (db)
      (execute
       (update (intern table-name)
               (set= :count n
                     :appearance_date date
                     :is_penalty (if is-penalty 1 0)
                     :wait_time wait-time)
               (where (:like :ipaddr ipaddr)))))))

(defun insert-posted-ipaddr-from-db (table-name ipaddr universal-time)
  (let ((date (get-current-datetime universal-time t 0)))
    (with-connection (db)
      (execute
       (insert-into (intern table-name)
                    (set= :ipaddr ipaddr
                          :appearance_date date
                          :is_penalty 0
                          :count 1
                          :wait_time *default-penalty-time*))))))

(defun get-detail-time-from-universal-time (time &optional (is-not-utc nil))
  (multiple-value-bind (second minute hour date month year day summer timezone)
      (if is-not-utc (decode-universal-time time 0) (decode-universal-time time))
    (declare (ignore day summer timezone))
    (list :year year :month month :date date :hour hour :minute minute :second second)))

(defmacro hour-to-second (h)
  `(* ,h 60 60))

(defun check-abuse-post (ipaddr time)
  (let* ((table-name "posted_ipaddr_table")
         (fetch-result (get-posted-ipaddr-values table-name ipaddr))
         (current-detail-date (get-detail-time-from-universal-time time t))
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
           (diff (abs (- left right))))
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
                   ((title :type '(:varchar 255)
                           :not-null t)
                    (create-date :type 'datetime
                                 :not-null t)
                    (last-modified-date :type 'datetime
                                        :not-null t)
                    (res-count :type 'integer
                               :not-null t
                               :default 1)
                    (unixtime :type 'integer
                              :primary-key t))))))

(defun format-datetime (date)
  (multiple-value-bind (second minute hour date month year day summer timezone)
      (decode-universal-time (+ date *9-hour-seconds*))
    (declare (ignore day summer timezone))
    (format nil "~A/~A/~A ~A:~A:~A" year month date hour minute second)))

(defun get-table-column-count (table-name column)
  (with-connection (db)
    (retrieve-one
     (select (fields (:count (intern column)))
             (from (intern table-name))))))

(defun check-exists-table-and-create-table-when-does-not ()
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

(defun create-dat (&key unixtime first-line)
  (let* ((filename (concatenate 'string (write-to-string unixtime) ".dat"))
         (path (concatenate 'string "dat/" filename)))
    (with-open-file (i (make-pathname :name path)
                       :direction :output
                       :if-does-not-exist :create
                       :element-type '(unsigned-byte 8))
      (let ((tmp (sb-ext:string-to-octets first-line :external-format :sjis)))
        (write-sequence tmp i)))))


(defun create-res (&key name trip-key email date text ipaddr (first nil) (title ""))
  (let* ((datetime (replace-hyphen-to-slash (get-current-datetime date)))
         (id (generate-id :ipaddr ipaddr :date datetime))
         (trip (if (and (stringp trip-key) (string/= trip-key ""))
                   (generate-trip (subseq trip-key 1 (length trip-key)) "utf8")
                   ""))
         (final-text (apply-dice (shape-text (replace-other-line-to-lf text)))))
    (when (string/= trip "")
      (setq trip (concatenate 'string "</b>" (string #\BLACK_DIAMOND) trip "<b>")))
    (if first
        (format nil "~A~A<>~A<>~A ID:~A<>~A<>~A~%" (apply-dice name t) trip email datetime id final-text title)
        (format nil "~A~A<>~A<>~A ID:~A<>~A<>~%" (apply-dice name t) trip email datetime id final-text))))

(defun create-thread-in-db (&key title create-date unixtime)
  (let ((date (get-current-datetime create-date)))
    (with-connection (db)
      (execute
       (insert-into :threads
                    (set= :title (escape-string title)
                          :create-date date
                          :last-modified-date date
                          :res-count 1
                          :unixtime unixtime))))))

(defun create-thread (&key _parsed date ipaddr)
  (check-exists-table-and-create-table-when-does-not)
  (let* ((title (get-value-from-key-on-list "subject" _parsed))
         (name (escape-string (get-value-from-key-on-list "FROM" _parsed)))
         (trip-key "")
         (text (escape-string (get-value-from-key-on-list "MESSAGE" _parsed)))
         (email (escape-string (get-value-from-key-on-list "mail" _parsed)))
         (unixtime (get-unix-time date)))
    (unless email
      (setq email ""))
    (unless text
      (setq text ""))
    (unless name
      (setq name *default-name*))
    (if (null (or (null title)
                  (null name)))
        (progn (let ((tmp (separate-trip-from-input name)))
                 (when (> (length tmp) 1)
                   (setq trip-key (cadr tmp)))
                 (setq name (car tmp)))
               (labels ((progress (&optional (count 0))
                          (handler-case (funcall (lambda (title date unixtime ipaddr name text)
                                                   (create-thread-in-db :title title :create-date date :unixtime unixtime)
                                                   (create-dat :unixtime unixtime
                                                               :first-line (create-res :name (escape-string name) :trip-key trip-key :email email :text text :ipaddr ipaddr :date date :first t :title title))
                                                   200)
                                                 title date unixtime ipaddr name text)
                            (error (e)
                              (declare (ignore e))
                                (incf unixtime)
                                (if (< count 10)
                                    (progress (incf count))
                                    (return-from create-thread 400))))))
                 (progress)))
        400)))

(defun insert-res (_parsed ipaddr universal-time)
  (let* ((bbs (get-value-from-key-on-list "bbs" _parsed))
         (key (get-value-from-key-on-list "key" _parsed))
         (time (get-value-from-key-on-list "time" _parsed))
         (from (get-value-from-key-on-list "FROM" _parsed))
         (mail (escape-string (get-value-from-key-on-list "mail" _parsed)))
         (message (escape-string (get-value-from-key-on-list "MESSAGE" _parsed)))
         (status 200))
    (unless from
      (setq from *default-name*))
    (if (or (null bbs)
            (null key)
            (null time)
            (null mail)
            (null message))
        400
        (progn (when (string= from "")
                 (setq from *default-name*))
               (when (and (string/= time "") (< (cadr (get-res-count :key key)) *default-max-length*)
                          (> (get-unix-time universal-time) (parse-integer time :radix 10)))
                 (with-open-file (input (concatenate 'string "dat/" key ".dat")
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
                          (res (create-res :name (escape-string name) :trip-key trip :email mail :text message :ipaddr ipaddr :date universal-time)))
                     (write-sequence (sb-ext:string-to-octets res :external-format :sjis) input)
                     (update-last-modified-date-of-thread :date universal-time :key key)
                     (update-res-count-of-thread :key key)
                     (when (>= (cadr (get-res-count :key key)) *default-max-length*)
                       (write-sequence (sb-ext:string-to-octets *1001* :external-format :sjis) input)
                       (update-res-count-of-thread :key key)
                       (update-last-modified-date-of-thread :date universal-time :key key)))))
               status))))

(defun put-thread-list (board-name)
  (if (string= board-name *board-name*)
      (progn
        (check-exists-table-and-create-table-when-does-not)
        (let ((result (get-thread-list)))
          (dolist (x result)
            (setf (getf x :create-date) (format-datetime (getf x :create-date)))
            (setf (getf x :last-modified-date) (format-datetime (getf x :last-modified-date))))
          (render #P"board.html" (list :here "sample dayo"
                                       :bbs board-name
                                       :time (get-unix-time (get-universal-time))
                                       :threads result))))
      (on-exception *web* 404)))


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

(defroute "/*.json" ()
  (setf (getf (response-headers *response*) :content-type) "application/json")
  (format t "~A~%" *response*)
  "{\"name\": \"Lain\"}")

(defroute ("/:board-name/" :method :GET) (&key board-name)
  ;; (maphash #'(lambda (key value) (format t "~%Key:~A, Value:~A~%" key value)) (request-headers *request*))
  ;; (print (gethash "cookie" (request-headers *request*)))
  (put-thread-list board-name))

(defroute ("/:board-name" :method :GET) (&key board-name)
  (put-thread-list board-name))

(defroute ("/:board-name/subject.txt" :method :GET) (&key board-name)
  (declare (ignore board-name))
  (let* ((tmp (get-thread-list-when-create-subject-txt))
         (result (list nil)))
    (if tmp
        (progn
          (dolist (x tmp)
            (let ((dat-name (generate-dat-name x))
                  (title (cadr (member :title x)))
                  (res-count (cadr (member :res-count x))))
              (push (format nil "~A<>~A (~A)~%" dat-name title res-count) result)))
          (let* ((final (apply #'concatenate 'string (cdr (reverse result))))
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
  (let* ((filepath (concatenate 'string "dat/" unixtime ".dat"))
         (dat-list (dat-to-keyword-list filepath))
         (title (cadr (member :title (car dat-list))))
         (current-unix-time (get-unix-time (get-universal-time))))
    (if (probe-file filepath)
        (render #P "thread.html" (list :title title :thread dat-list :bbs *board-name* :key unixtime :time current-unix-time))
        (on-exception *web* 404))))

(defun get-param (body)
  (let ((tmp (cl-ppcre:split "&" body))
        (result (list nil)))
    (dolist (x tmp)
      (push (cl-ppcre:split "=" x) result))
    (cdr (reverse result))))

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

(defun check-exist-session (session-id ipaddr)
  (let ((tmp (with-connection (db)
               (retrieve-one
                (select :session_id
                        (from :session_data))))))
    tmp))

(defun add-session-data (session-id ipaddr)
  (with-connection (db)
    (execute
     (insert-into :session_data
                  (set= :session_id session-id
                        :ipaddr ipaddr)))))

(defun update-last-modified-date-of-thread (&key date key)
  (with-connection (db)
    (execute
     (update :threads
             (set= :last-modified-date (get-current-datetime date 0))
             (where (:like :unixtime key))))))

(defun get-res-count (&key key)
  (with-connection (db)
    (retrieve-one
     (select :res-count
             (from :threads)
             (where (:like :unixtime key))))))

(defun update-res-count-of-thread (&key key)
  (let ((tmp (get-res-count :key key)))
    (with-connection (db)
      (execute
       (update :threads
               (set= :res-count (1+  (cadr tmp)))
               (where (:like :unixtime key)))))))


;; response-headers is slot of *response*. *response* are type of struct. example: respoinse-cookies.
;; (setf (getf (response-headers *response*) :set-cookie) (concatenate 'string "PON=" ipaddr))

;; <form action="/test/bbs.cgi" method="post">
;;     <label for="FROM">名前：</label>
;;     <input name="FROM" type="text" value="名無しさん"/>
;;     <label for="mail">メール：</label>
;;     <input name="mail" type="text" value=""/>
;;     <input name="bbs" type="hidden" value="{{ bbs }}"/>
;;     <input name="key" type="hidden" value="{{ key }}"/>
;;     <input name="time" type="hidden" value="{{ time }}"/>
;;     <input name="submit" type="hidden" value="書き込む"/>
;;     <label for="MESSAGE">本文：</label>
;;     <textarea cols="30" name="MESSAGE" rows="10"></textarea>
;;     <button type="submit">送信</button>
;; </form>


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
         (parsed-param nil)
         (form (list nil)))
    (unless (null decoded-query)
      (setq parsed-param (get-param decoded-query))
      (dolist (x parsed-param)
        (let ((key (car x))
              (value (cadr x)))
          (if (null value)
              (setq form (nconc (list key nil) form))
              (setq form (nconc (list key (try-url-decode value)) form))))))
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
                 (setf (response-status *response*) 302))
               (progn
                 (setf (response-status *response*) status))))
         (next-route))
        ((string= submit "新規スレッド作成")
         (let ((status (create-thread :_parsed form :date universal-time :ipaddr ipaddr)))
           (if (= status 200)
               (progn (setf (getf (response-headers *response*) :location) (concatenate 'string "/" bbs))
                      (setf (response-status *response*) 302))
               (setf (response-status *response*) status)))
         (next-route))
        (t
         (setf (response-status *response*) 400)
         (next-route))))))

(defroute ("/test/bbs.cgi" :method :POST) (&key _parsed)
  (let ((ipaddr (caveman2:request-remote-addr caveman2:*request*))
        (universal-time (get-universal-time)))
    (if (check-abuse-post ipaddr universal-time)
        (let* ((message (replace-not-available-char-when-cp932 (get-value-from-key "MESSAGE" _parsed)))
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
        (progn (setf (response-status *response*) 429)
               (render #P "time_restrict.html" (list :minute
                                                     (/ (getf (get-posted-ipaddr-values "posted_ipaddr_table" ipaddr)
                                                              :wait-time)
                                                        60)
                                                     
                                                     :bbs (cdr (assoc "bbs" _parsed :test #'string=))
                                                     :key (cdr (assoc "key" _parsed :test #'string=))))))))

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
          (setf (response-status *response*) 404)
          nil))
    (handler-case (flexi-streams:octets-to-string tmp :external-format :utf-8)
      (error (e)
        (declare (ignore e))
        (flexi-streams:octets-to-string tmp)))))

(defroute ("/:board-name/dat/:unixtime.dat" :method :GET) (&key board-name unixtime)
  (declare (ignore board-name))
  (let ((pathname (probe-file (concatenate 'string "dat/" unixtime ".dat"))))
    (if (not (null pathname))
        (progn
          (setf (getf (response-headers *response*) :content-type) "text/plain; charset=Shift_jis")
          (setf (response-body *response*) pathname))
        (progn
          (setf (response-status *response*) 404)
          ""))))


(defroute ("/:board-name/SETTING.TXT" :method :GET) (&key board-name)
  (if (string= board-name *board-name*)
      (let ((pathname "SETTING.txt"))
        (setf (getf (response-headers *response*) :content-type) "text/plain; charset=Shift_jis")
        (setf (response-body *response*) (probe-file pathname)))
      (on-exception *web* 404)))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
