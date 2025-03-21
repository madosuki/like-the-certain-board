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
                :write-log
                :check-whether-integer
                :separate-numbers-from-key-for-kako
                :detect-monazilla)
  (:import-from :like-certain-board.config
                :*confirmed-key*)
  (:import-from :lack.middleware.csrf
   :csrf-token)
  (:import-from :cl-ppcre
                :scan)
  (:export :*web*))
(in-package :like-certain-board.web)

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defmethod make-response ((app <web>) &optional status header body)
  (let ((res (call-next-method)))
    (setf (getf (response-headers res) :X-Content-Type-Options) "nosniff")
    res))
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

;;
;; Routing rules

(defroute ("/" :method :GET) ()
  (let ((board-list-data (handler-case (get-board-list)
                           (error (e)
                             (declare (ignore e))
                             nil)
                           (:no-error (v) v))))
    (if board-list-data
        (index-view board-list-data)
        (notfound-view))))

(defroute ("/about" :method :GET) ()
  (about-page-view (format nil "~A/about" *https-root-path*)))

(defroute ("/:board-name/" :method :GET) (&key board-name)
  (let ((view (process-on-root-of-board *web* *session* board-name)))
    (if view
        view
        (notfound-view))))

(defroute ("/:board-name" :method :GET) (&key board-name)
  (let ((view (process-on-root-of-board *web* *session* board-name)))
    (if view
        view
        (notfound-view))))

(defroute ("/:board-name/kakolog" :method :GET) (&key board-name)
  (let* ((board-data (get-a-board-name-from-name board-name))
         (data (if board-data
                   (get-kakolog-thread-list (getf board-data :id))
                   nil))
         (url (format nil "~A/~A/kakolog" *https-root-path* board-name)))
    (if data
        (kakolog-list-view board-name url data)
        (kakolog-list-view board-name url))))

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

(defroute ("/test/read.cgi/:board-name/:unixtime" :method :GET) (&key board-name unixtime)
  (let* ((dat-filepath (format nil "~A/~A/~A.dat" *dat-path* board-name unixtime))
         (is-login (gethash *session-login-key* *session*))
         (board-data (get-a-board-name-from-name board-name))
         (is-number (check-whether-integer unixtime)))
    (if (and board-data (eq is-number :integer-string))
        (if (probe-file dat-filepath)
            (let* ((dat-list (dat-to-keyword-list dat-filepath))
                   (title (cadr (member :title (car dat-list))))
                   (current-unix-time (get-unix-time (get-universal-time))))
              (thread-view :title title
                           :thread dat-list
                           :bbs board-name
                           :key unixtime
                           :time current-unix-time
                           :csrf-token (csrf-token *session*)
                           :is-login is-login
                           :url (format nil "~A/test/read.cgi/~A/~A"
                                        *https-root-path*
                                        board-name
                                        unixtime)))
            (let* ((separated (separate-numbers-from-key-for-kako unixtime))
                   (html-path (if (or (eq separated :small) (eq separated :not-numbers))
                                  nil
                                  (format nil "~A/~A/~A/~A/~A.html"
                                          *kakolog-html-path* board-name
                                          (car separated) (cdr separated)
                                          unixtime))))
              (if (probe-file html-path)
                  (progn
                    (setf (getf (response-headers *response*) :location)
                          (format nil "/~A/kako/~A/~A/~A.html"
                                  board-name (car separated) (cdr separated) unixtime))
                    (set-response-status 302)
                    (next-route))
                  (on-exception *web* 404))))
        (on-exception *web* 404))))

(defroute ("/:board-name/kako/:four-digit-numbers/:five-digit-numbers/:unixtime.html" :method :GET) (&key board-name four-digit-numbers five-digit-numbers unixtime)
  (let ((path (format nil "~A/~A/~A/~A/~A.html" *kakolog-html-path* board-name four-digit-numbers five-digit-numbers unixtime))
        (board-data (get-a-board-name-from-name board-name))
        (is-number-for-unixtime (check-whether-integer unixtime))
        (is-number-for-four-digit-numbers (check-whether-integer four-digit-numbers))
        (is-number-for-five-digit-numbers (check-whether-integer five-digit-numbers)))
    (if (and (not (null board-data))
             (eq is-number-for-unixtime :integer-string)
             (eq is-number-for-four-digit-numbers :integer-string)
             (eq is-number-for-five-digit-numbers :integer-string)
             (probe-file path))
        (let* ((data (get-a-kakolog-thread unixtime (getf board-data :id)))
               (title (if data
                          (getf data :title)
                          nil)))
          (if title
              (kakolog-view :title title
                            :html-path path
                            :board-url-name board-name
                            :first four-digit-numbers
                            :second five-digit-numbers
                            :key unixtime
                            :url (format nil "~A/~A/kako/~A/~A/~A.html"
                                         *https-root-path* board-name
                                         four-digit-numbers five-digit-numbers
                                         unixtime))
              (on-exception *web* 404)))
        (on-exception *web* 404))))

(defroute ("/test/bbs.cgi" :method :POST) (&key _parsed)
  (let* ((ipaddr (caveman2:request-remote-addr caveman2:*request*))
         (universal-time (get-universal-time))
         (current-unixtime (get-unix-time universal-time))
         (user-agent (gethash "user-agent" (caveman2:request-headers caveman2:*request*)))
         (is-monazilla (detect-monazilla user-agent))
         (bbs (cdr (assoc "bbs" _parsed :test #'string=)))
         (key (cdr (assoc "key" _parsed :test #'string=)))
         (board-data (if bbs
                         (get-a-board-name-from-name bbs)
                         nil))
         (is-confirmed (gethash *confirmed-key* *session*)))
    (cond ((or (null user-agent))
           (set-response-status 400)
           (write-result-view :mode :error :message "bad parameter"))
          ((and (null board-data) (null key))
           (set-response-status 400)
           (write-result-view :mode :error :message "bad parameter"))
          ((and user-agent *session* board-data)
           (let ((check-abuse-result (check-abuse-post :current-unixtime current-unixtime
                                                       :user-agent user-agent
                                                       :ipaddr ipaddr
                                                       :session *session*)))
             (cond ((null check-abuse-result)
                    (set-response-status 400)
                    "")
                   (t
                    (let ((check-status (try-update-restrict-time-info ipaddr current-unixtime _parsed is-confirmed is-monazilla check-abuse-result)))
                      (cond ((or (eq check-status :ok)
                                 (eq check-status :over-24)
                                 (eq check-status :first))
                             (let* ((message (get-value-from-key "MESSAGE" _parsed))
                                    (raw-body (request-raw-body *request*))
                                    (content-length (request-content-length *request*))
                                    (tmp-array (make-array content-length :adjustable t :fill-pointer content-length))
                                    (is-e nil))
                               (format t "content-length: ~A~%" content-length)
                               (handler-case (read-sequence tmp-array raw-body)
                                 (error (e)
                                   (write-log :mode :error
                                              :message (format nil "Error read squence in bbs.cgi: ~A" e))
                                   (setq is-e t)))
                               (if is-e
                                   (write-result-view :error-type :something :message "bad parameter")
                                   (handler-case  (bbs-cgi-function tmp-array ipaddr universal-time is-monazilla *session*)
                                     (error (e)
                                       (set-response-status 400)
                                       (write-log :mode :error
                                                  :message (format nil "Error in bbs-cgi-function: ~A" e))
                                       (write-result-view :error-type :something :message "bad parameter"))))))
                            ((null check-status)
                             (set-response-status 400)
                             (write-result-view :error-type :something :message "bad parameter"))
                            (t
                             (let ((thread (if (eq (check-whether-integer key) :integer-string)
                                               (get-a-thread key (getf board-data :id))
                                               nil)))
                               (if thread
                                   (progn (set-response-status 200)
                                          (if is-monazilla
                                              (time-error-msg (if (eq check-status :restrict-24)
                                                                  *time-error-24h-msg*
                                                                  *time-error-10sec-msg*))
                                              (time-restrict-view
                                               :mode check-status
                                               :bbs bbs
                                               :key key
                                               :mail *admin-mailaddr*)))
                                   (progn (set-response-status 200)
                                          (if is-monazilla
                                              (time-error-msg (if (eq check-status :restrict-24)
                                                                  *time-error-24h-msg*
                                                                  *time-error-10sec-msg*))
                                              (time-restrict-view
                                               :mode check-status
                                               :bbs bbs
                                               :mail *admin-mailaddr*))))))))))))
          (t
           (set-response-status 400)
           (write-result-view :error-type :something :message "bad parameter")))))



(defroute ("/:board-name/dat/:unixtime.dat" :method :GET) (&key board-name unixtime)
  (let ((path (format nil "~A/~A/~A.dat" *dat-path* board-name unixtime))
        (board-data (get-a-board-name-from-name board-name))
        (is-number (check-whether-integer unixtime)))
    (if (and (not (null board-data))
             (eq is-number :integer-string))
        (if (probe-file path)
            (progn (setf (getf (response-headers *response*) :content-type) "text/plain; charset=Shift_jis")
                   (setf (response-body *response*) (pathname path)))
            (let ((separated (separate-numbers-from-key-for-kako unixtime)))
              (if (or (eq separated :small) (eq separated :not-numbers))
                  (progn (set-response-status 404)
                         "")
                  (progn
                    (setf (getf (response-headers *response*) :location)
                          (format nil "/~A/kako/~A/~A/~A.dat"
                                  board-name (car separated) (cdr separated) unixtime))
                    (set-response-status 302)
                    (next-route)))))
        (progn
          (set-response-status 404)
          ""))))

(defroute ("/:board-name/kako/:four-digit-numbers/:five-digit-numbers/:unixtime.dat" :method :GET) (&key board-name four-digit-numbers five-digit-numbers unixtime)
  (let ((path (format nil "~A/~A/~A/~A/~A.dat" *kakolog-dat-path* board-name four-digit-numbers five-digit-numbers unixtime))
        (board-data (get-a-board-name-from-name board-name))
        (is-number-for-unixtime (check-whether-integer unixtime))
        (is-number-for-four-digit-numbers (check-whether-integer four-digit-numbers))
        (is-number-for-five-digit-numbers (check-whether-integer five-digit-numbers)))
    (if (and (not (null board-data))
             (eq is-number-for-unixtime :integer-string)
             (eq is-number-for-four-digit-numbers :integer-string)
             (eq is-number-for-five-digit-numbers :integer-string)
             (probe-file path))
        (progn
          (setf (getf (response-headers *response*) :content-type) "text/plain; charset=Shift_jis")
          (setf (response-body *response*) (pathname path))
          (next-route))
        (on-exception *web* 404))))

(defroute ("/:board-name/SETTING.TXT" :method :GET) (&key board-name)
  (let ((board-data (get-a-board-name-from-name board-name)))
    (if board-data
        (let ((pathname (format nil "~A/~A/SETTING.txt" *settings-path* board-name)))
          (setf (getf (response-headers *response*) :content-type) "text/plain; charset=Shift_jis")
          (setf (response-body *response*) (probe-file pathname)))
        (on-exception *web* 404))))

(defroute ("/:board-name/login" :method :GET) (&key board-name _parsed)
  (let ((board-data (get-a-board-name-from-name board-name))
        (user-agent (gethash "user-agent" (caveman2:request-headers caveman2:*request*)))
        (condition (get-value-from-key "condition" _parsed)))
    (cond ((null board-data)
           (on-exception *web* 404))
          ((or (detect-monazilla user-agent)
               (/= 443 (request-server-port *request*))
               (null board-data))
           (set-response-status 403)
           "Forbidden")
          (t
           (login-view :board-url-name board-name
                       :board-name (getf board-data :name)
                       :csrf-token (csrf-token *session*)
                       :is-login (let ((hash (gethash *session-login-key* *session*)))
                                   (cond ((and condition (null hash))
                                          :failed)
                                         (hash
                                          :success)
                                         (t
                                          nil))))))))

(defroute ("/:board-name/create-user" :method :GET) (&key board-name)
  (let* ((board-data (get-a-board-name-from-name board-name))
         (user-agent (gethash "user-agent" (caveman2:request-headers caveman2:*request*)))
         (is-monazilla (detect-monazilla user-agent))
         (is-login (gethash *session-login-key* *session*))
         (is-admin (gethash *session-admin-key* *session*)))
    (cond ((or (null user-agent)
               (null board-data)
               is-monazilla
               (/= 443 (request-server-port *request*))
               (null is-login)
               (null is-admin))
           (set-response-status 403)
           "Forbidden")
          (t
           (create-user-view :board-url-name board-name
                             :board-name (getf board-data :name)
                             :csrf-token (csrf-token *session*))))))

(defroute ("/:board-name/user-list" :method :GET) (&key board-name)
  (let* ((board-data (get-a-board-name-from-name board-name))
         (user-agent (gethash "user-agent" (caveman2:request-headers caveman2:*request*)))
         (is-monazilla (detect-monazilla user-agent))
         (is-login (gethash *session-login-key* *session*))
         (is-admin (gethash *session-admin-key* *session*)))
    (if (or (null user-agent)
            (null board-data)
            is-monazilla
            (/= 443 (request-server-port *request*))
            (null is-login)
            (null is-admin))
        (progn (set-response-status 403)
               "Forbidden")
        (let ((user-list (get-user-list (getf board-data :id))))
          (user-list-view :board-name (getf board-data :name)
                          :board-url-name board-name
                          :user-list user-list
                          :csrf-token (csrf-token *session*))))))


(defroute ("/:board-name/api/user" :method :POST) (&key board-name _parsed)
  (let* ((mode (get-value-from-key "mode" _parsed))
         (user-name (get-value-from-key "user_name" _parsed))
         (password (get-value-from-key "password" _parsed))
         (is-admin (get-value-from-key "is_admin" _parsed))
         (cap-text (get-value-from-key "cap_text" _parsed))
         (date (get-universal-time))
         (board-data (get-a-board-name-from-name board-name))
         (user-agent (gethash "user-agent" (caveman2:request-headers caveman2:*request*))))
    (when user-name
      (setq user-name (replace-not-available-char-when-cp932 (convert-html-special-chars user-name))))
    (cond ((null board-data)
           (on-exception *web* 404))
          ((null mode)
           (set-response-status 400)
           "invalid parameter")
          ((or (detect-monazilla user-agent) (/= 443 (request-server-port *request*)))
           (set-response-status 403)
           "Forbidden")
          ((equal mode "login")
           (if (or (null user-name)
                   (null password))
               (progn
                 (set-response-status 400)
                 "invalid parameter")
               (progn
                 (let ((login-check (login (getf board-data :id) user-name password date *session*)))
                   (cond ((eq login-check :logged-in)
                          (login-view :board-name (getf board-data :name)
                                      :board-url-name board-name
                                      :is-login :logged-in))
                         ((eq login-check :success)
                          (setf (getf (response-headers *response*) :location)
                                (concatenate 'string "/" board-name))
                          (set-response-status 302)
                          "success!")
                         (t
                          (login-view :board-name (getf board-data :name)
                                      :board-url-name board-name
                                      :is-login :failed)
                          (setf (getf (response-headers *response*) :location)
                                (format nil "/~A/login?condition=failed" board-name))
                          (set-response-status 302)
                          "failed login"))))))
          ((equal mode "logout")
           (let ((check (gethash *session-login-key* *session*)))
             (when check
               (clrhash *session*)
               (setf (getf (getf (request-env *request*) :lack.session.options) :expire) t))
             (setf (getf (response-headers *response*) :location)
                   (concatenate 'string "/" board-name))
             (set-response-status 302)
             (next-route)))
          ((equal mode "create")
           (if (and (gethash *session-login-key* *session*) (gethash *session-admin-key* *session*))
               (let ((create-result (create-user (getf board-data :id) user-name password date nil cap-text)))
                 (set-response-status 400)
                 (cond ((eq create-result :exist-user)
                        "This user name is existed.")
                       ((eq create-result :create-failed)
                        "Error: failed create account.")
                       ((eql create-result :success)
                        (set-response-status 200)
                        "Finish Create that Account.")
                       (t
                        "invalid parameter.")))
               (progn
                 (set-response-status 403)
                 "Forbidden")))
          ((equal mode "delete_self")
           (if (and (gethash *session-login-key* *session*) (gethash *session-user-id* *session*))
               (let* ((board-id (getf board-data :id))
                      (is-exists-user (get-user-table-from-id board-id user-id)))
                 (if is-exists-user
                     (let ((is-success (handler-case (progn (delete-user-from-id board-id (getf is-exists-user :id))
                                                            :success)
                                         (error (e)
                                           (declare (ignore e))
                                           :failed))))
                       (if (eq :success is-success)
                           (progn
                             (set-response-status 302)
                             (setf (getf (response-headers *response*) :location)
                                   (format nil "~A/~A/user-list"
                                           *https-root-path*
                                           board-name))
                             "Success")
                           "Failed"))
                     "Failed"))
               (progn
                 (set-response-status 403)
                 "Forbidden")))
          ((equal mode "delete_other")
           (if (and (gethash *session-login-key* *session*) (gethash *session-admin-key* *session*) (gethash *session-user-id*))
               (let* ((board-id (getf board-data :id))
                      (is-exists-user (get-user-table board-id user-name))
                      (admin (get-user-table-from-id board-id (gethash *session-user-id*)))
                      (is-admin (if admin
                                    (getf admin :is-admin)
                                    nil)))
                 (if (and is-admin is-exists-user)
                     (let ((is-success (handler-case (progn (delete-user-from-id board-id (getf is-exists-user :id))
                                                            :success)
                                         (error (e)
                                           (declare (ignore e))
                                           :failed))))
                       (if (eq :success is-success)
                           (progn
                             (set-response-status 302)
                             (setf (getf (response-headers *response*) :location)
                                   (format nil "~A/~A/user-list"
                                           *https-root-path*
                                           board-name))
                             "Success")
                           "Failed"))
                     "Failed"))
               (progn
                 (set-response-status 403)
                 "Forbidden")))
          (t
           (set-response-status 400)
           "invalid parameter"))))


(defroute ("/:board-name/api/line" :method :POST) (&key board-name _parsed)
  (let* ((key (get-value-from-key "key" _parsed))
         (check-key (check-whether-integer key))
         (line (get-value-from-key "line" _parsed))
         (mode (get-value-from-key "mode" _parsed))
         (line-number (if (eq check-key :integer-string)
                          (parse-integer line :junk-allowed t)
                          nil))
         (is-login (gethash *session-login-key* *session*))
         (is-admin (gethash *session-admin-key* *session*))
         (board-data (get-a-board-name-from-name board-name))
         (user-agent (gethash "user-agent" (caveman2:request-headers caveman2:*request*))))
    (cond ((null board-data)
           (on-exception *web* 404))
          ((detect-monazilla user-agent)
           (set-response-status 403)
           "Forbidden")
          ((/= 443 (request-server-port *request*))
           (set-response-status 403)
           "Forbidden")
          ((null is-login)
           (set-response-status 403)
           "Forbidden")
          ((null is-admin)
           (set-response-status 403)
           "Forbidden")
          ((or (null line)
               (null key)
               (not (eq check-key :integer-string)))
           (set-response-status 400)
           "invalid param")
          ((numberp line-number)
           (cond ((string= mode "delete")
                  (if (delete-line-in-dat key board-name line-number)
                      (progn
                        (set-response-status 302)
                        (setf (getf (response-headers *response*) :location)
                              (format nil "~A/test/read.cgi/~A/~A" *https-root-path* board-name key))
                        "success")
                      "faild delete."))
                 (t
                  "unknown mode")))
          (t
           (on-exception *web* 404)))))

(defroute ("/:board-name/api/thread" :method :POST) (&key board-name _parsed)
  (let ((key (get-value-from-key "key" _parsed))
        (mode (get-value-from-key "mode" _parsed))
        (is-login (gethash *session-login-key* *session*))
        (is-admin (gethash *session-admin-key* *session*))
        (board-data (get-a-board-name-from-name board-name))
        (user-agent (gethash "user-agent" (caveman2:request-headers caveman2:*request*))))
    (cond ((or (null mode)
               (null board-data)
               (/= 443 (request-server-port *request*))
               (detect-monazilla user-agent)
               (null is-login)
               (null is-admin))
           (set-response-status 403)
           "Forbidden")
          ((string= mode "delete")
           (if (and (not (null key))
                    (eq (check-whether-integer key) :integer-string)
                    (check-exist-row (parse-integer key :junk-allowed t)))
               (let ((filepath (format nil "~A/~A/~A.dat" *dat-path* board-name key)))
                 (when (probe-file filepath)
                   (handler-case
                       (progn
                         (delete-thread key (getf board-data :id))
                         (delete-file filepath))
                     (error (e)
                       (declare (ignore e))
                       (set-response-status 400)
                       "Failed")
                     (:no-error ()
                       (setf (getf (response-headers *response*) :location) (concatenate 'string "/" board-name))
                       (set-response-status 302)
                       (next-route)))))
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
             (if (and (not (null key))
                      (eq (check-whether-integer key) :integer-string)
                      (not (null title)))
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


