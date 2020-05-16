(ql:quickload :clack)
(ql:quickload :cl-fad)

(defparameter *server* nil)

(defun end-server ()
  (clack:stop *server*))

(defun run (name)
    (let ((application name))
    (if (cl-fad:file-exists-p application)
        (progn
          (unwind-protect
               (setq *server* (clack:clackup application :port 8080))
            (end-server)))
        (format t "not found this file."))))

;; (defun main (&rest argv)
;;   (let ((tmp (car argv)))
;;     (run tmp)))

(defun main ()
  (run "./app.lisp"))
