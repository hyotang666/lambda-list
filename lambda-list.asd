; vim: ft=lisp et
(in-package :asdf)
(defsystem :lambda-list
  :depends-on (:type-ext :millet)
  :components ((:file "lambda-list")))
;; Perform method below is added by JINGOH.GENERATOR.
(defmethod perform ((o test-op) (c (eql (find-system "lambda-list"))))
 (test-system :lambda-list.test))