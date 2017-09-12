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
;; Perform method below is added by JINGOH.GENERATOR.
(defmethod perform ((o test-op) (c (eql (find-system "lambda-list"))))
 (test-system :lambda-list.test))
