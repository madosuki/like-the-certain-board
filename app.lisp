(ql:quickload :like-certain-board)

(defpackage like-certain-board.app
  (:use :cl)
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
                :connection-settings))

(in-package :like-certain-board.app)

;; (defclass <my-lack-middleware-state-cookie> (<lack.session.state.cookie>)
;;   ((path :init-form "/"
;;          :accessor path)
;;    (domain :init-form nil
;;            :accessor domain)
;;    (expiers :init-form (get-universal-time)
;;             :accessor)
;;    (secure :init-form nil
;;            :accessor secure)
;;    (httponly :init-form nil
;;              :accessor httponly)
;;    (cookie-key :init-form "lack.session"
;;                :accessor cookie-key)))

;; (defclass <my-lack-middleware-session> (<lack.middleware.session>)
;;   ((state :init-form (make-instance '<my-lack-session-state-cookie>)
;;           :accessor state)))

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
 :session
 ;; (:session
 ;;  :state (make-instance <my-lack-middleware-state-cooki>))
 (if (productionp)
     nil
     (lambda (app)
       (lambda (env)
         (let ((datafly:*trace-sql* t))
           (funcall app env)))))
 *web*)
