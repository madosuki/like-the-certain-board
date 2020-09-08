(defsystem "like-certain-board"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ("clack"
               "lack"
               "caveman2"
               "envy"
               "cl-ppcre"
               "uiop"
               "quri"

               "cffi"

               ;; for @route annotation
               "cl-syntax-annot"

               ;; HTML Template
               "djula"

               ;; for DB
               "datafly"
               "sxql"
               "cl-dbi"

               ;; otherwise
               "alexandria"

               ;; hash and crypt algorithm
               "ironclad"

               ;; session
               "lack-session-store-dbi"

               ;; utility
               "cl-fad"

               ;; board string generator
               "generate-like-certain-board-strings"

               "trivial-shell"

               )
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("config" "view" "db"))
                 (:file "web" :depends-on ("view"))
                 (:file "view" :depends-on ("config"))
                 (:file "db" :depends-on ("config"))
                 (:file "config"))))
  :description ""
  :in-order-to ((test-op (test-op "like-certain-board-test"))))
