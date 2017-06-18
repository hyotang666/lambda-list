; vim: ft=lisp et
(in-package :asdf)
(defsystem :lambda-list.test :depends-on (:jingoh "lambda-list") :components
 ((:file "lambda-list")) :perform
 (test-op (o c) (symbol-call :jingoh :examine :lambda-list)))
