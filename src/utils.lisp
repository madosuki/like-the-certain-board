(in-package :cl-user)
(defpackage like-certain-board.utils
  (:use :cl)
  (:import-from :like-certain-board.config
   :*log-path*)
  (:export
   :write-log))
(in-package :like-certain-board.utils)

(defun write-file (&key filename message)
  (with-open-file (o (format nil "~A/~A" *log-path* filename)
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :append)
    (write-line message o)))

(defun write-log (&key mode message)
  (if (equal *log-path* "")
      (format t "~%~A~%" message)
      (cond ((eq mode :error)
             (write-file :filename "error.txt"
                         :message message))
            ((eq mode :change-result)
             (write-file :filename "changes.txt"
                         :message message)))))
