(in-package :cl-user)
(defpackage like-certain-board.utils
  (:use :cl)
  (:import-from :like-certain-board.config
   :*log-path*)
  (:export
   :write-log
   :separate-numbers-from-key-for-kako
   :check-whether-integer))
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

(defun check-whether-integer (n)
  (typecase n
    (integer :integer)
    (t (let ((a (parse-integer n :junk-allowed t)))
         (if a
             :integer-string
             :otherwise)))))


(defun separate-numbers-from-key-for-kako (key)
  (let ((s (if (eq (check-whether-integer key) :integer)
               (write-to-string key)
               key)))
    (unless (> (length s) 9)
      (return-from separate-numbers-from-key-for-kako :small))
    (let ((1st (parse-integer (subseq s 0 4)
                              :junk-allowed t))
          (2nd (parse-integer (subseq s 0 5)
                              :junk-allowed t)))
      (if (or (null 1st)
              (null 2nd))
          :not-numbers
          (cons 1st 2nd)))))



