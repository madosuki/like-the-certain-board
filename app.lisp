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
                :connection-settings))

(in-package :like-certain-board.app)

;; (defstruct (extend-cookie-state (:include cookie-state))
;;   (samesite "Lax" :type string))

;; (defmethod finalize-state ((state extend-cookie-state) sid (res list) options)
;;   ;; Don't send Set-Cookie header when it's not necessary.
;;   (destructuring-bind (&key no-store new-session change-id expire &allow-other-keys)
;;       options
;;     (when (or no-store
;;               (not (or new-session change-id expire)))
;;       (return-from finalize-state res)))

;;   (let ((res (apply #'make-response res))
;;         (options (with-slots (path domain expires secure httponly samesite) state
;;                    (list :path path
;;                          :domain domain
;;                          :secure secure
;;                          :httponly httponly
;;                          :samesite samesite
;;                          :expires (+ (get-universal-time)
;;                                      (getf options :expires expires))))))
;;     (setf (getf (response-set-cookies res) (cookie-state-cookie-key state))
;;           `(:value ,sid ,@options))
;;     (finalize-response res)))


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
          :expires 1800))
 (if (productionp)
     nil
     (lambda (app)
       (lambda (env)
         (let ((datafly:*trace-sql* t))
           (funcall app env)))))
 *web*)
