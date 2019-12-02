(in-package :cl-user)
(defpackage like-certain-board.web
  (:use :cl
        :caveman2
        :like-certain-board.config
        :like-certain-board.view
        :like-certain-board.db
        :datafly
        :sxql
        :generate-like-certain-board-strings)
  (:export :*web*))
(in-package :like-certain-board.web)

(defvar *default-name* "名無しさん")

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

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

(defun create-res (&key name trip-key email date text ipaddr (first nil) (title ""))
  (let* ((datetime (get-current-datetime date 0))
         (id (generate-id :ipaddr ipaddr :date datetime))
         (trip (if (and (stringp trip-key) (string/= trip-key ""))
                   (generate-trip trip-key "utf8")
                   ""))
         (final-text (replace-newline-to-br-tag text)))
    (when (string/= trip "")
      (setq trip (concatenate 'string "</b>" (string #\BLACK_DIAMOND) trip "<b>")))
    (if first
        (format nil "~A~A<>~A<>~A ~A<>~A<>~A~%" name trip email datetime id final-text title)
        (format nil "~A~A<>~A<>~A ~A<>~A<>~%" name trip email datetime id final-text))))

(defun create-thread (&key title create-date unixtime)
  (let ((date (get-current-datetime create-date 0))
        (id (get-table-count "threads")))
    (with-connection (db)
      (execute
       (insert-into :threads
                    (set= :title title
                          :create-date date
                          :last-modified-date date
                          :res-count 1
                          :id (if (numberp (cadr id))
                                  (1+ (cadr id))
                                  1)
                          :unixtime unixtime))))))

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

(defroute ("/board" :method :GET) ()
  (check-exists-table-and-create-table-when-does-not)
  (let ((result (get-thread-list)))
    (dolist (x result)
      (setf (getf x :create-date) (format-datetime (getf x :create-date)))
      (setf (getf x :last-modified-date) (format-datetime (getf x :last-modified-date))))
    (render #P"board.html" (list :here "sample dayo" :threads result))))

(defmacro get-value-from-key (key target)
  `(cdr (assoc ,key ,target :test #'string=)))

(defun create-dat (&key unixtime first-line)
  (let* ((filename (concatenate 'string (write-to-string unixtime) ".dat"))
         (path (concatenate 'string "dat/" filename)))
    (with-open-file (i (make-pathname :name path)
                       :direction :output
                       :if-does-not-exist :create)
      (write-string first-line i))))

(defroute ("/board" :method :POST) (&key _parsed)
  (check-exists-table-and-create-table-when-does-not)
  (let* ((title (get-value-from-key "title" _parsed))
         (name (get-value-from-key "name" _parsed))
         (trip-key "")
         (text (get-value-from-key "text" _parsed))
         (email (get-value-from-key "email" _parsed))
         (date (get-universal-time))
         (ipaddr (caveman2:request-remote-addr caveman2:*request*))
         (unixtime (get-unix-time date)))
    (unless email
      (setq email ""))
    (unless text
      (setq text ""))
    (if (null (or (null title)
                  (null name)))
        (progn (let ((tmp (separate-trip-from-input name)))
                 (when (> (length tmp) 0)
                   (setq trip-key (cadr tmp)))
                 (setq name (car tmp)))
               (create-thread :title title :create-date date :unixtime unixtime)
               (create-dat :unixtime unixtime
                           :first-line (create-res :name name :trip-key trip-key :email email :text text :ipaddr ipaddr :date date :first t :title title))
               (setf (getf (response-headers *response*) :location) "http://localhost:8000/board")
               (setf (response-status *response*) 301)
               ;; (format t "~A~%~A~%" *response* (response-status *response*))
               (next-route))
        (progn
          (setf (response-status *response*) 400)
          (next-route)))))

(defmacro generate-dat-name (l)
  `(concatenate 'string (write-to-string (cadr (member :unixtime ,l))) ".dat"))

(defroute ("/board/subject.txt" :method :GET) ()
  (let* ((tmp (get-thread-list-when-create-subject-txt))
         (result (list nil)))
    (dolist (x tmp)
      (let ((dat-name (generate-dat-name x))
            (title (cadr (member :title x)))
            (res-count (cadr (member :res-count x))))
        (push (format nil "~A<>~A (~A)~%" dat-name title res-count) result)))
    (apply #'concatenate 'string (cdr (reverse result)))))

(defroute "/ipinfo" ()
  (caveman2:request-remote-addr caveman2:*request*))

(defroute ("/thread/:unixtime" :method :GET) (&key unixtime)
  (let* ((filepath (concatenate 'string "dat/" unixtime ".dat"))
         (dat-list (dat-to-keyword-list filepath))
         (title (cadr (member :title (car dat-list)))))
    (if (probe-file filepath)
        (render #P "thread.html" (list :title title :thread dat-list))
        (on-exception *web* 404))))

(defroute ("/test/bbs.cgi" :method :POST) (&key _parsed)
  (let ((bbs (get-value-from-key "bbs" _parsed))
        (key (get-value-from-key "key" _parsed))
        (submit (get-value-from-key "submit" _parsed))
        (from (get-value-from-key "from" _parsed))
        (mail (get-value-from-key "mail" _parsed))
        (message (get-value-from-key "message" _parsed)))
    (unless (or (null bbs)
                (null key)
                (null submit)
                (null from)
                (null mail)
                (null message))
      (when (string= from "")
        (setq from *default-name*)))
    (setf (getf (response-headers *response*) :location) (concatenate 'string "/thread/" key))
    (setf (response-status *response*) 301)
    (next-route)))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
