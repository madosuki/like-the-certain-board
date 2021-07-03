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
;; (defmethod make-response ((app <web>) &optional status header body)
;;   (let ((res (call-next-method)))
;;     (setf (getf (response-headers res) :X-Content-Type-Options) "nosniff")
;;     res))
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

;;
;; Routing rules

(defroute ("/" :method :GET) ()
  (let ((board-list-data (get-board-list)))
    (format t "~%~A~%" (caveman2:request-remote-addr caveman2:*request*))
    (index-view board-list-data)))

(defroute ("/about" :method :GET) ()
  (about-page-view (format nil "~A/about" *http-root-path*)))

(defroute ("/:board-name/" :method :GET) (&key board-name)
  (let ((board-data (get-a-board-name-from-name board-name)))
    (if board-data
        (put-thread-list board-name (getf board-data :name) *web* (format nil "~A/~A" *http-root-path* board-name) (csrf-token *session*))
        (on-exception *web* 404))))

(defroute ("/:board-name" :method :GET) (&key board-name)
  (let ((board-data (get-a-board-name-from-name board-name)))
    (if board-data
        (put-thread-list board-name (getf board-data :name) *web* (format nil "~A/~A" *http-root-path* board-name) (csrf-token *session*))
        (on-exception *web* 404))))

(defroute ("/:board-name/kakolog" :method :GET) (&key board-name)
  (let* ((board-data (get-a-board-name-from-name board-name))
         (data (if board-data
                   (get-kakolog-thread-list (getf board-data :id))
                   nil))
         (url (format nil "~A/~A/kakolog" *http-root-path* board-name)))
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
                                        *http-root-path*
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
                                         *http-root-path* board-name
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
                         nil)))
    (cond ((null user-agent)
           (set-response-status 403)
           "")
          ((and (null board-data) (null key))
           (set-response-status 400)
           "")
          ((and user-agent *session* board-data)
           (let ((check-abuse-result (check-abuse-post :current-unixtime current-unixtime
                                                       :user-agent user-agent
                                                       :ipaddr ipaddr
                                                       :session *session*)))
             (if (eq check-abuse-result :ok)
                 (let* ((message (get-value-from-key "MESSAGE" _parsed))
                        (cookie (gethash "cookie" (request-headers *request*)))
                        (splited-cookie (if (null cookie)
                                            nil
                                            (mapcar #'(lambda (v) (cl-ppcre:split "=" v))
                                                    (cl-ppcre:split ";" cookie))))
                        (raw-body (request-raw-body *request*))
                        (content-length (request-content-length *request*))
                        (tmp-array (make-array content-length :adjustable t :fill-pointer content-length))
                        (is-e nil))
                   (handler-case (read-sequence tmp-array raw-body)
                     (error (e)
                       (write-log :mode :error
                                  :message (format nil "Error read squence in bbs.cgi: ~A" e))
                       (setq is-e t)))
                   (if is-e
                       (write-result-view :error-type 'something :message "bad parameter")
                       (handler-case  (bbs-cgi-function tmp-array ipaddr universal-time)
                         (error (e)
                           (write-log :mode :error
                                      :message (format nil "Error in bbs-cgi-function: ~A" e))
                           (write-result-view :error-type 'something :message "bad parameter")))))
                 (let ((thread (if (eq (check-whether-integer key) :integer-string)
                                   (get-a-thread key (getf board-data :id))
                                   nil)))
                   (if thread
                       (progn (set-response-status 200)
                              (if is-monazilla
                                  (time-error-msg *time-error-10sec-msg*)
                                  (time-restrict-view
                                   :mode check-abuse-result
                                   :bbs bbs
                                   :key key
                                   :mail *admin-mailaddr*)))
                       (progn (set-response-status 200)
                              (if is-monazilla
                                  (time-error-msg *time-error-24h-msg*)
                                  (time-restrict-view
                                   :mode check-abuse-result
                                   :bbs bbs
                                   :mail *admin-mailaddr*))))))))
          (t
           (set-response-status 403)
           "403 Forbidden"))))



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
    (if (and board-data (null (detect-monazilla user-agent)))
        (login-view :board-url-name board-name
                    :board-name (getf board-data :name)
                    :csrf-token (csrf-token *session*)
                    :is-login (let ((hash (gethash *session-login-key* *session*)))
                                (cond ((and condition (null hash))
                                       :failed)
                                      (hash
                                       :success)
                                      (t
                                       nil))))
        (on-exception *web* 404))))


(defroute ("/:board-name/api/user" :method :POST) (&key board-name _parsed)
  (let* ((mode (get-value-from-key "mode" _parsed))
         (user-name (get-value-from-key "user_name" _parsed))
         (password (get-value-from-key "password" _parsed))
         (is-admin (get-value-from-key "is_admin" _parsed))
         (cap-text (get-value-from-key "cap_text" _parsed))
         ;; (cookie (gethash "cookie" (request-headers *request*)))
         ;; (splited-cookie (if (null cookie)
         ;;                     nil
         ;;                     (mapcar #'(lambda (v) (cl-ppcre:split "=" v))
         ;;                             (cl-ppcre:split ";" cookie))))
         (date (get-universal-time))
         (board-data (get-a-board-name-from-name board-name))
         (user-agent (gethash "user-agent" (caveman2:request-headers caveman2:*request*))))
    (when user-name
      (setq user-name (replace-not-available-char-when-cp932 (convert-html-special-chars user-name))))
    (cond ((or (null board-data) (null mode))
           (set-response-status 400)
           "invalid parameter")
          ((detect-monazilla user-agent)
           (set-response-status 403)
           "Forbidden")
          ((equal mode "login")
           (if (or (null user-name)
                   (null password))
               (progn
                 (set-response-status 400)
                 "invalid parameter")
               (progn
                 (let ((login-check (login (getf board-data :id) user-name password date)))
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
               (clrhash *session*))
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
                       ((eql create-result t)
                        (set-response-status 200)
                        "Finish Create that Account.")
                       (t
                        "invalid parameter.")))
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
           (if (delete-line-in-dat key board-name line-number)
               (progn
                 (set-response-status 302)
                 (setf (getf (response-headers *response*) :location)
                       (format nil "/test/read.cgi/~A/~A" board-name key))
                 (next-route))
               "faild delete."))
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
               (detect-monazilla user-agent)
               (null is-login)
               (null is-admin))
           (set-response-status 403)
           "Access Denied")
          ((string= mode "delete")
           (if (and (not (null key))
                    (eq (check-whether-integer key) :integer-string)
                    (check-exist-row (parse-integer key :junk-allowed t)))
               (let ((filepath (format nil "~A/~A/~A.dat" *dat-path* board-name key)))
                 (when (probe-file filepath)
                   (delete-thread key (getf board-data :id))
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
