(ql:quickload 'clack)
(ql:quickload 'cl-fad)

(defparameter *server* nil)

(defun run (name)
    (let ((application name))
    (if (cl-fad:file-exists-p application)
        (progn
          (setq *server* (clack:clackup application)))
        (format t "not found this file."))))

(defun end-server ()
  (clack:stop *server*))

;; (defun main (&rest argv)
;;   (let ((tmp (car argv)))
;;     (run tmp)))

(defun main ()
  (run "/root/.roswell/local-projects/like-the-certain-board/app.lisp"))
