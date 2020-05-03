(defsystem "like-certain-board-test"
  :defsystem-depends-on ("prove-asdf")
  :author ""
  :license ""
  :depends-on ("like-certain-board"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "like-certain-board"))))
  :description "Test system for like-certain-board"
  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
