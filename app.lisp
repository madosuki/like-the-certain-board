(ql:quickload :like-certain-board)

(defpackage like-certain-board.app
  (:use :cl :lack.middleware.session.state.cookie)
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
 ;; :session
 (:session
  :state (make-cookie-state
          :httponly t
          :cookie-key "app.session"
          :samesite :lax
          :expires 1800))
 (if (productionp)
     nil
     (lambda (app)
       (lambda (env)
         (let ((datafly:*trace-sql* t))
           (funcall app env)))))
 *web*)
