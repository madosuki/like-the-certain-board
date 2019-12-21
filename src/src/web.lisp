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
     (select :* (from :threads)))))

(defun get-thread-list-when-create-subject-txt ()
  (with-connection (db)
    (retrieve-all
     (select (fields :title :res-count :unixtime)
             (from :threads)))))

(defun check-exists-table (table-name)
  (with-connection (db)
    (retrieve-one
     (select :*
             (from (intern table-name))))))

(defun init-threads-table ()
  (with-connection (db)
    (execute
     (create-table (:threads :if-exists-not t)
                   ((id :type 'integer
                        :primary-key t)
                    (title :type '(:varchar 255)
                           :not-null t)
                    (create-date :type 'datetime
                                 :not-null t)
                    (last-modified-date :type 'datetime
                                        :not-null t)
                    (res-count :type 'integer
                               :not-null t
                               :default 1)
                    (unixtime :type 'integer
                              :not-null t))))))

(defun format-datetime (date)
  (multiple-value-bind (second minute hour date month year day summer timezone)
      (decode-universal-time date 0)
    (declare (ignore day summer timezone))
    (format nil "~A/~A/~A ~A:~A:~A" year month date hour minute second)))

(defun get-table-count (table-name)
  (with-connection (db)
    (retrieve-one
     (select (fields (:count :id))
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
  (let* ((datetime (replace-hyphen-to-slash (get-current-datetime date 0)))
         (id (generate-id :ipaddr ipaddr :date datetime))
         (trip (if (and (stringp trip-key) (string/= trip-key ""))
                   (generate-trip (subseq trip-key 1 (length trip-key)) "utf8")
                   ""))
         (final-text (shape-text (replace-other-line-to-lf text))))
    (when (string/= trip "")
      (setq trip (concatenate 'string "</b>" (string #\BLACK_DIAMOND) trip "<b>")))
    (if first
        (format nil "~A~A<>~A<>~A ID:~A<>~A<>~A~%" name trip email datetime id final-text title)
        (format nil "~A~A<>~A<>~A ID:~A<>~A<>~%" name trip email datetime id final-text))))

(defun create-thread-in-db (&key title create-date unixtime)
  (let ((date (get-current-datetime create-date 0))
        (id (get-table-count "threads")))
    (with-connection (db)
      (execute
       (insert-into :threads
                    (set= :title (escape-string title)
                          :create-date date
                          :last-modified-date date
                          :res-count 1
                          :id (if (numberp (cadr id))
                                  (1+ (cadr id))
                                  1)
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
      (setq *default-name*))
    (if (null (or (null title)
                  (null name)))
        (progn (let ((tmp (separate-trip-from-input name)))
                 (when (> (length tmp) 1)
                   (setq trip-key (cadr tmp)))
                 (setq name (car tmp)))
               (create-thread-in-db :title title :create-date date :unixtime unixtime)
               (create-dat :unixtime unixtime
                           :first-line (create-res :name (escape-string name) :trip-key trip-key :email email :text text :ipaddr ipaddr :date date :first t :title title))
               200)
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
               (when (and (string/= time "")
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
                     (write-sequence (sb-ext:string-to-octets res :external-format :sjis) input))))
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
    (dolist (x tmp)
      (let ((dat-name (generate-dat-name x))
            (title (cadr (member :title x)))
            (res-count (cadr (member :res-count x))))
        (push (format nil "~A<>~A (~A)~%" dat-name title res-count) result)))
    (let* ((final (apply #'concatenate 'string (cdr (reverse result))))
           (oct (sb-ext:string-to-octets final :external-format :sjis))
           (content-length (length oct)))
      `(200 (:content-type "text/plain" :content-length ,content-length) ,oct))))

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

(defun try-url-decode (x &optional (encode :CP932) (error-count 0))
  (handler-case (quri:url-decode x :encoding encode)
    (error (e)
      (declare (ignore e))
      (case error-count
        (0
         (try-url-decode x :UTF-8 1))
        (1
         (try-url-decode x :EUC-JP 2))
        (2
         (try-url-decode x :ASCII 3))
        (otherwise
         nil)))))


;; response-headers is slot of *response*. *response* are type of struct. example: respoinse-cookies.
;; (setf (getf (response-headers *response*) :set-cookie) (concatenate 'string "PON=" ipaddr))

(defroute ("/test/bbs.cgi" :method :POST) (&key _parsed)
  (let* ((ipaddr (caveman2:request-remote-addr caveman2:*request*))
         (universal-time (get-universal-time))
         (message (replace-not-available-char-when-cp932 (get-value-from-key "MESSAGE" _parsed)))
         (cookie (gethash "cookie" (request-headers *request*)))
         (splited-cookie (if (null cookie)
                             nil
                             (mapcar #'(lambda (v) (cl-ppcre:split "=" v))
                                     (cl-ppcre:split ";" cookie))))
         (raw-body (request-raw-body *request*))
         (content-length (request-content-length *request*))
         (tmp-array (make-array content-length :adjustable t :fill-pointer content-length))
         (form (list nil)))
    (read-sequence tmp-array raw-body)
    (labels ((try-decode-bytes (arr &optional (encode :CP932) (error-count 0))
               (handler-case (sb-ext:octets-to-string arr :external-format encode)
                 (error (e)
                   (declare (ignore))
                   (case error-count
                     (0
                      (try-decode-bytes arr :UTF-8 1))
                     (1
                      (try-decode-bytes arr :EUC-JP 2))
                     (2
                      (try-decode-bytes arr :ASCII 3))
                     (otherwise
                      nil))))))
      (let* ((decoded-query (try-decode-bytes (coerce tmp-array '(vector (unsigned-byte 8)))))
             (parsed-param nil))
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
          (print parsed-param)
          (cond ((string= submit "書き込む")
                 (let ((status (insert-res form ipaddr universal-time)))
                   (if (= status 200)
                       (progn
                         (setf (getf (response-headers *response*) :location) (concatenate 'string "/test/read.cgi/" bbs "/" key))
                         (setf (response-status *response*) 200))
                       (progn
                         (setf (response-status *response*) status)))))
                ((string= submit "新規スレッド作成")
                 (let ((status (create-thread :_parsed form :date universal-time :ipaddr ipaddr)))
                   (if (= status 200)
                       (progn (setf (getf (response-headers *response*) :location) (concatenate 'string "/" bbs))
                              (setf (response-status *response*) 200))
                       (setf (response-status *response*) status))))
                (t
                 (setf (response-status *response*) 400))))
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
