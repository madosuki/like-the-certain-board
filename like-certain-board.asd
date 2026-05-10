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

               "cl-json"

               ;; for @route annotation
               "cl-syntax-annot"

               ;; HTML Template
               ;; "djula"
               "cl-markup"

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
               "lack-middleware-csrf"
               "lack-middleware-session"

               ;; utility
               "cl-fad"

               ;; board string generator. ref: https://github.com/madosuki/generate-like-certain-board-strings
               ;; this library expect installed to path. like ros install generate-like-certain-board-strings
               "generate-like-certain-board-strings"

               ;; libargon2 wrapper. ref: https://github.com/madosuki/cl-argon2-cffi
               ;; this library expect installed to path. like ros install cl-argon2-cffi
               "cl-argon2-cffi"

               "trivial-shell"

               "cl-string-generator"

               )
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("config" "view" "db"))
                 (:file "web" :depends-on ("view" "webfunctions" "utils"))
                 (:file "view" :depends-on ("config" "db" "utils"))
                 (:file "db" :depends-on ("config"))
                 (:file "config")
                 (:file "webfunctions" :depends-on ("utils" "db"))
                 (:file "utils" :depends-on ("config")))))
  :description ""
  :in-order-to ((test-op (test-op "like-certain-board-test"))))
