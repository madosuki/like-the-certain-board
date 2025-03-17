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
  ;; :store (make-dbi-store :connector (lambda ()
  ;;                                     (apply #'dbi:connect
  ;;                                            (like-certain-board.db:connection-settings))))
  :state (make-cookie-state
          :httponly t
          :cookie-key "session" ;; default value is lack.session
          :samesite :strict
          :secure t
          :expires 1800))
 (:csrf
  :one-time t
  ;; block-app section is set behaviour when passed invalid csrf token.
  :block-app (lambda (app env)
               (declare (ignore app)
                        (ignore env))
               '(400
                 (:content-type "text/plain"
                  :content-length 9)
                 ("bad request"))
               ;; (let ((user-agent (gethash "user-agent" (getf env :headers))))
               ;;   (if (cl-ppcre:scan "^Monazilla/1.00" user-agent)
               ;;       (funcall app env)
               ;;       '(400
               ;;         (:content-type "text/plain"
               ;;          :content-length 9)
               ;;         ("bad request"))
               ;;       ))
               ))
 (if (productionp)
     nil
     (lambda (app)
       (lambda (env)
         (let ((datafly:*trace-sql* t))
           (funcall app env)))))
 *web*)
