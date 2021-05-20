(in-package :cl-user)
(defpackage like-certain-board.web
  (:use :cl
        :caveman2
        :like-certain-board.config
        :like-certain-board.view
        :like-certain-board.db
        :like-certain-board.webfunctions
        :quri
        :cl-fad
        :generate-like-certain-board-strings)
  (:import-from :like-certain-board.utils
                :write-log)
  (:export :*web*))
(in-package :like-certain-board.web)

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)



;;
;; Routing rules

(defroute "/" ()
  (let ((board-list-data (get-board-list)))
    (index-view board-list-data)))

(defroute ("/:board-name/" :method :GET) (&key board-name)
  (let ((board-data (get-a-board-name-from-name board-name)))
    (if board-data
        (put-thread-list board-name (getf board-data :name) *web*)
        (on-exception *web* 404))))

(defroute ("/:board-name" :method :GET) (&key board-name)
  (let ((board-data (get-a-board-name-from-name board-name)))
    (if board-data
        (put-thread-list board-name (getf board-data :name) *web*)
        (on-exception *web* 404))))

(defroute ("/:board-name/kakolog" :method :GET) (&key board-name)
  (let* ((board-data (get-a-board-name-from-name board-name))
         (data (if board-data
                   (get-kakolog-thread-list (getf board-data :id))
                   nil)))
    (if data
        (kakolog-list-view board-name data)
        (kakolog-list-view board-name))))

(defroute ("/:board-name/subject.txt" :method :GET) (&key board-name)
  (let ((board-data (get-a-board-name-from-name board-name)))
    (if board-data
        (let* ((tmp (get-thread-list-when-create-subject-txt (getf board-data :id)))
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
                `(200 (:content-type "text/plain" :content-length ,content-length) ,empty))))
        (on-exception *web* 404))))

(defroute "/ipinfo" ()
  (caveman2:request-remote-addr caveman2:*request*))

(defroute ("/test/read.cgi/:board-name/:unixtime" :method :GET) (&key board-name unixtime)
  (let* ((dat-filepath (format nil "~A/~A/~A.dat" *dat-path* board-name unixtime))
         (is-login (gethash *session-login-key* *session*))
         (board-data (get-a-board-name-from-name board-name)))
    (if board-data
        (if (probe-file dat-filepath)
            (let* ((dat-list (dat-to-keyword-list dat-filepath))
                   (title (cadr (member :title (car dat-list))))
                   (current-unix-time (get-unix-time (get-universal-time))))
              (thread-view :title title
                           :thread dat-list
                           :bbs board-name
                           :key unixtime
                           :time current-unix-time
                           :is-login is-login))
            (let ((html-path (format nil "~A/~A/~A.html" *kakolog-html-path* board-name unixtime)))
              (if (probe-file html-path)
                  (progn
                    (setf (getf (response-headers *response*) :location)
                          (format nil "/~A/kakolog/~A" board-name unixtime))
                    (set-response-status 302)
                    (next-route))
                  (on-exception *web* 404))))
        (on-exception *web* 404))))

(defroute ("/:board-name/kakolog/:unixtime" :method :GET) (&key board-name unixtime)
  (let ((path (format nil "~A/~A/~A.html" *kakolog-html-path* board-name unixtime))
        (board-data (get-a-board-name-from-name board-name)))
    (if (and (not (null board-data))
             (probe-file path))
        (let* ((data (get-a-kakolog-thread unixtime (getf board-data :id)))
               (title (if data
                          (getf data :title)
                          nil)))
          (if title
              (kakolog-view title path board-name unixtime)
              (on-exception *web* 404)))
        (on-exception *web* 404))))


(defroute ("/test/bbs.cgi" :method :POST) (&key _parsed)
  (let* ((ipaddr (caveman2:request-remote-addr caveman2:*request*))
         (last-post-seconds (gethash *session-last-post-seconds* *session*))
         (universal-time (get-universal-time))
         (current-unixtime (get-unix-time universal-time)))
    (if (check-abuse-post last-post-seconds current-unixtime)
        (let* ((message (get-value-from-key "MESSAGE" _parsed))
               (cookie (gethash "cookie" (request-headers *request*)))
               (splited-cookie (if (null cookie)
                                   nil
                                   (mapcar #'(lambda (v) (cl-ppcre:split "=" v))
                                           (cl-ppcre:split ";" cookie))))
               (raw-body (request-raw-body *request*))
               (content-length (request-content-length *request*))
               (tmp-array (make-array content-length :adjustable t :fill-pointer content-length)))
          (setf (gethash *session-last-post-seconds* *session*) current-unixtime)
          (handler-case  (progn (read-sequence tmp-array raw-body)
                                (bbs-cgi-function tmp-array ipaddr universal-time))
            (error (e)
              (write-log :mode :error
                         :message e)
              (write-result-view :error-type 'something :message "bad parameter"))))
        (let ((bbs (cdr (assoc "bbs" _parsed :test #'string=)))
              (key (cdr (assoc "key" _parsed :test #'string=))))
          (if (and (null bbs) (null key))
              (progn (set-response-status 400)
                     (write-result-view :error-type 'something :message "bad parameter: require key and bbs."))
              (progn (set-response-status 429)
                     (time-restrict-view
                      :ipaddr ipaddr
                      :minute 1
                      :bbs bbs
                      :key (if key key nil)
                      :mail "example@example.com")
                     ))))))



(defroute ("/:board-name/dat/:unixtime.dat" :method :GET) (&key board-name unixtime)
  (let ((pathname (probe-file (format nil "~A/~A/~A.dat" *dat-path* board-name unixtime)))
        (board-data (get-a-board-name-from-name board-name)))
    (if (and (not (null board-data))
             (not (null pathname)))
        (progn
          (setf (getf (response-headers *response*) :content-type) "text/plain; charset=Shift_jis")
          (setf (response-body *response*) pathname))
        (progn
          (set-response-status 404)
          ""))))

(defroute ("/:board-name/kakolog/dat/:unixtime.dat" :method :GET) (&key board-name unixtime)
  (let ((path (probe-file (format nil "~A/~A/~A.dat" *kakolog-dat-path* board-name unixtime)))
        (board-data (get-a-board-name-from-name board-name)))
    (if (and (not (null board-data))
             (not (null path)))
        (progn
          (setf (getf (response-headers *response*) :content-type) "text/plain; charset=Shift_jis")
          (setf (response-body *response*) path))
        (on-exception *web* 404))))

(defroute ("/:board-name/SETTING.TXT" :method :GET) (&key board-name)
  (let ((board-data (get-a-board-name-from-name board-name)))
    (if board-data
        (let ((pathname (format nil "~A/~A/SETTING.txt" *settings-path* board-name)))
          (setf (getf (response-headers *response*) :content-type) "text/plain; charset=Shift_jis")
          (setf (response-body *response*) (probe-file pathname)))
        (on-exception *web* 404))))

(defroute ("/:board-name/login" :method :GET) (&key board-name)
  (let ((board-data (get-a-board-name-from-name board-name)))
    (if board-data
        (login-view :board-url-name board-name
                    :board-name (getf board-data :name)
                    :is-login (if (gethash *session-login-key* *session*)
                                  'logged-in
                                  nil))
        (on-exception *web* 404))))

(defroute ("/:board-name/api/user" :method :POST) (&key board-name _parsed)
  (let* ((ipaddr (caveman2:request-remote-addr caveman2:*request*))
         (mode (get-value-from-key "mode" _parsed))
         (user-name (get-value-from-key "user_name" _parsed))
         (password (get-value-from-key "password" _parsed))
         (is-admin (get-value-from-key "is_admin" _parsed))
         (cap-text (get-value-from-key "cap_text" _parsed))
         (cookie (gethash "cookie" (request-headers *request*)))
         (splited-cookie (if (null cookie)
                             nil
                             (mapcar #'(lambda (v) (cl-ppcre:split "=" v))
                                     (cl-ppcre:split ";" cookie))))
         (date (get-universal-time))
         (board-data (get-a-board-name-from-name board-name)))
    (cond ((null board-data)
           (on-exception *web* 404))
          ((or (null mode) (null (string= mode "login")) (null (string= mode "logout")))
           (set-response-status 400)
           (next-route))
          ((equal mode "login")
           (if (or (null user-name)
                   (null password))
               (progn
                 (set-response-status 400)
                 (next-route))
               (progn
                 (setq user-name (create-safety-strings user-name))
                 (let ((login-check (login board-name user-name password date)))
                   (cond ((eq login-check 'logged-in)
                          (login-view :board-name (getf board-data :name) :board-url-name board-name :is-login 'logged-in))
                         ((eq login-check 'success)
                          (setf (getf (response-headers *response*) :location) (concatenate 'string "/" board-name))
                          (set-response-status 302)
                          (next-route))
                         (t
                          (set-response-status 401)
                          (login-view :board-name (getf board-data :name) :board-url-name board-name :is-login 'failed)
                          ))))))
           ((equal mode "logout")
            (let ((check (gethash *session-login-key* *session*)))
              (when check
                (clrhash *session*))
              (setf (getf (response-headers *response*) :location)
                    (concatenate 'string "/" board-name))
              (set-response-status 302)
              (next-route)
              ))
           ((equal mode "create")
            (if (string= ipaddr *admin-ipaddr*)
                (let ((create-result (create-user board-name user-name password date is-admin cap-text)))
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
                  (set-response-status 400)
                  "Your ip aadress is don't trust.")))
           (t
            (set-response-status 400)
            "not defined that mode"))))


(defroute ("/:board-name/api/line" :method :POST) (&key board-name _parsed)
  (let* ((key (get-value-from-key "key" _parsed))
         (line (get-value-from-key "line" _parsed))
         (line-number (parse-integer (if (stringp line) line "") :junk-allowed t))
         (is-login (gethash *session-login-key* *session*))
         (is-admin (gethash *session-admin-key* *session*))
         (board-data (get-a-board-name-from-name board-name)))
    (cond ((null board-data)
           (on-exception *web* 404))
          ((null is-login)
           (set-response-status 403)
           "not was logged-in")
          ((null is-admin)
           (set-response-status 403)
           "you are not admin")
          ((or (null line) (null key))
           (set-response-status 400)
           "invalid param")
          ((numberp line-number)
           (if (delete-line-in-dat key line-number)
                 (let* ((filepath (format nil "~A/~A/~A.dat" *dat-path* board-name key))
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
        (mode (get-value-from-key "mode" _parsed))
        (is-login (gethash *session-login-key* *session*))
        (is-admin (gethash *session-admin-key* *session*))
        (board-data (get-a-board-name-from-name board-name)))
    (cond ((or (null mode)
               (null board-data)
               ;; (null is-login)
               ;; (null is-admin)
               )
           (set-response-status 403)
           "Access Denied")
          ((string= mode "delete")
           (if (and (not (null key))
                    (check-exist-row (parse-integer key)))
               (let ((filepath (format nil "~A/~A/~A.dat" *dat-path* board-name key)))
                 (when (probe-file filepath)
                   (delete-thread key board-name)
                   (delete-file filepath)
                   (setf (getf (response-headers *response*) :location) (concatenate 'string "/" board-name))
                   (set-response-status 302)
                   (next-route)))
               (prgon
                (set-response-status 403)
                "invalid params")))
          ((string= mode "log")
           (let* ((board-id (let ((board-data (get-a-board-name-from-name board-name)))
                              (if board-data
                                  (getf board-data :id)
                                  nil)))
                  (thread-data (if board-id
                                   (get-a-thread (parse-integer key) board-id)
                                   nil))
                  (title (if thread-data
                             (getf thread-data :title)
                             nil)))
             (if (and (not (null key)) (not (null title)))
                 (format nil "~A" (kakolog-process :key key :title title :board-url-name board-name :board-id board-id))
                 (progn (set-response-status 400)
                        "invalid params"))))
          ((string= mode "convert-bunch-of-thread-to-kakolog")
           (let ((result (convert-bunch-of-thread-to-kakolog (getf board-data :id))))
             (if result
                 (progn
                   (dolist (x result)
                     (write-log :mode :change-result
                                :message (format nil "~A: ~A" (car x) (cdr x))))
                   "success")
                 (progn
                   (write-log :mode :change-result
                              :message "failed bunch of thread to kakolog")
                   "failed"))))
          (t
           (set-response-status 400)
           "invalid params"))))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (notfound-view))
