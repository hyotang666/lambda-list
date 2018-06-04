; vim: ft=lisp et
(in-package :asdf)
(defsystem :lambda-list
  :author "Shinichi Sato"
  :description "Tiny lambda var collector."
  :long-description #.(read-file-string(subpathname *load-pathname*
                                                    "README.md"))
  :license "MIT"
  :depends-on (:type-ext :millet "closer-mop")
  :components ((:file "lambda-list")))

(defmethod component-depends-on ((o test-op) (c (eql (find-system "lambda-list"))))
  (append (call-next-method)'((test-op "lambda-list.test"))))
