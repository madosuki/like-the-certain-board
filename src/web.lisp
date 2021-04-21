(in-package :cl-user)
(defpackage like-certain-board.web
  (:use :cl
        :caveman2
        :like-certain-board.config
        :like-certain-board.view
        :like-certain-board.db
        :like-certain-board.webfunctions
        :datafly
        :sxql
        :quri
        :cl-fad
        :generate-like-certain-board-strings)
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
  (render #P"index.html"))

(defroute ("/:board-name/" :method :GET) (&key board-name)
  ;; (maphash #'(lambda (key value) (format t "~%Key:~A, Value:~A~%" key value)) (request-headers *request*))
  ;; (print (gethash "cookie" (request-headers *request*)))
  (put-thread-list board-name *web*))

(defroute ("/:board-name" :method :GET) (&key board-name)
  (put-thread-list board-name *web*))

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
          (read-sequence tmp-array raw-body)
          (bbs-cgi-function tmp-array ipaddr universal-time))
        (let ((bbs (cdr (assoc "bbs" _parsed :test #'string=)))
              (key (cdr (assoc "key" _parsed :test #'string=))))
          (if (or (null bbs) (null key))
              (progn (set-response-status 400)
                     (next-route))
              (set-response-status 429)
              (render #P "time_restrict.html" (list
                                               :ipaddr ipaddr
                                               :minute 1
                                               :bbs bbs
                                               :key (if key key nil))))))))




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
         (is-logout (get-value-from-key "is_logout" _parsed))
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
    (cond ((and (not (null is-login))
                (not (null is-logout)))
           (set-response-status 400)
           (next-route))
          ((not (null is-login))
           (if (or (null user-name)
                   (null password))
               (progn
                 (set-response-status 400)
                 (next-route))
               (progn
                 (setq user-name (create-safety-strings user-name))
                 (let ((login-check (login board-name user-name password ipaddr date)))
                   (cond ((eq login-check 'logged-in)
                          (render #P "login.html" (list :bbs board-name :is-login "logged-in")))
                         ((eql login-check t)
                          (setf (getf (response-headers *response*) :location) (concatenate 'string "/" board-name))
                          (set-response-status 302)
                          (next-route))
                         (t
                          (set-response-status 401)
                          (render #P "login.html" (list :bbs board-name :is-login "failed"))))))))
           ((not (null is-logout))
            (let ((check (gethash *session-login-key* *session*)))
              (when check
                (clrhash *session*))
              (setf (getf (response-headers *response*) :location)
                    (concatenate 'string "/" board-name))
              (set-response-status 302)
              (next-route)
              ))
           (t
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
                  (set-response-status 400)
                  "Your ip aadress is don't trust."))))))


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
