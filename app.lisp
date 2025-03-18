(ql:quickload :like-certain-board)

(defpackage like-certain-board.app
  (:use :cl :lack.middleware.session.state.cookie :lack.session.store.dbi)
  (:import-from :lack.builder
   :builder)
  (:import-from :ppcre
                :scan
                :regex-replace)
  (:import-from :like-certain-board.web
                :*web*)
  (:import-from :like-certain-board.config
                :config
                :productionp
   :*static-directory*)
  (:import-from :like-certain-board.db
   :connection-settings)
  (:import-from :lack.response
   :make-response
   :finalize-response
   :response-set-cookies))

(in-package :like-certain-board.app)

(builder
 (:static
  :path (lambda (path)
          (if (ppcre:scan "^(?:/images/|/css/|/js/|/robot\\.txt$|/favicon\\.ico$)" path)
              path
              nil))
  :root *static-directory*)
 (if (productionp)
     nil
     :accesslog)
 (if (getf (config) :error-log)
     `(:backtrace
       :output ,(getf (config) :error-log))
     nil)
 (:session
  ;; fixme: need close connection per access when postgresql.
  :store (make-dbi-store :connector (lambda ()
                                      (apply #'dbi:connect
                                             (like-certain-board.db:connection-settings)))
                         :disconnector #'dbi:disconnect)
  :state (make-cookie-state
          :httponly t
          :cookie-key "session" ;; default value is lack.session
          :samesite :strict
          :secure t
          :expires 1800))
 (:csrf
  :one-time t
  :block-app (lambda (app env)
               (let ((user-agent (gethash "user-agent" (getf env :headers))))
                 ;; this mean is non block if user agent is Monazilla.
                 (if (cl-ppcre:scan "^Monazilla/1.00" user-agent)
                     (funcall app env)
                     '(400
                       (:content-type "text/plain"
                        :content-length 11)
                       ("bad request"))
                     ))))
 (if (productionp)
     nil
     (lambda (app)
       (lambda (env)
         (let ((datafly:*trace-sql* t))
           (funcall app env)))))
 *web*)
