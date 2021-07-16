(in-package :cl-user)
(defpackage like-certain-board.utils
  (:use :cl)
  (:import-from :like-certain-board.config
   :*log-path*)
  (:import-from :cl-ppcre
   :scan)
  (:import-from :generate-like-certain-board-strings
   :get-unix-time
   :get-current-datetime)
  (:export
   :write-log
   :separate-numbers-from-key-for-kako
   :check-whether-integer
   :detect-monazilla))
(in-package :like-certain-board.utils)


(defmacro msg-format (stream msg)
  `(format ,stream "~%webapp(~A): ~A~%"
           (get-current-datetime (get-universal-time))
           ,msg))

(defun load-text-file (filename)
  (unless (probe-file filename)
    (return-from load-text-file '(:not-exists . nil)))
  (with-open-file (i filename
                     :direction :input)
    (cons :some
          (format nil "~{~A~}"
                  (loop for l = (read-line i nil)
                        while l
                        collect l)))))

(defun write-file (&key filename message)
  (with-open-file (o (format nil "~A/~A" *log-path* filename)
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :append)
    (write-line message o)))

(defun write-log (&key mode message)
  (if (equal *log-path* "")
      (msg-format t message)
      (cond ((eq mode :error)
             (write-file :filename "error.txt"
                         :message (msg-format nil message)))
            ((eq mode :change-result)
             (write-file :filename "changes.txt"
                         :message (msg-format nil message))))))

(defun check-whether-integer (n)
  (typecase n
    (integer :integer)
    (string (let ((a (scan "^(?!0)[0-9]+$" n)))
              (if a
                  :integer-string
                  :otherwise-string)))
    (t :otherwise)))


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


(defun detect-monazilla (user-agent)
  (not (null (scan "^Monazilla/1.00" user-agent))))
