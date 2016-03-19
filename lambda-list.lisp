(in-package :cl-user)
(defpackage :lambda-list (:use :cl :type-ext)
  (:export
    #:lambda-list
    #:vars<=lambda-list
    #:canonicalize-lambda-list
    #:lambda-list-keyword-p
    ))
(in-package :lambda-list)

(eval-when(:compile-toplevel :load-toplevel)
  (defun doc(path)
    (uiop:read-file-string
      (uiop:subpathname (or *compile-file-truename*
			    *load-truename*)
			path))))

(prototype lambda-list((or symbol function (cons (eql lambda) T)))
	   (values list &optional))
(defun lambda-list(arg)
  #.(doc "doc/lambda-list.md")
  #.(or ; to avoid write #-(or ...)
      #+clisp '(ext:arglist arg)
      #+ccl '(ccl:arglist (or(and(typep arg '(cons (eql lambda) T))
			       (coerce arg 'function))
			    arg))
      #+ecl '(ext:function-lambda-list(or(and(typep arg '(cons (eql lambda)T))
					   (coerce arg 'function))
					arg))
      #+sbcl '(sb-kernel:%fun-lambda-list(or(and(symbolp arg)
					      (macro-function arg))
					   (coerce arg 'function)))
      )) ; NIL as default.

(define-simple-type(symbols (:element-type symbol)
			    (:element-predicate symbolp)))

(prototype vars<=lambda-list(list &key(:as(member :function :method :macro)))
	   symbols)
(defun vars<=lambda-list(lambda-list &key(as :function))
  #.(doc "doc/vars<=lambda-list.md")
  (labels((rec(list &optional acc context)
	    (if(endp list)
	      (nreverse acc)
	      (let((elt(car list)))
		(etypecase elt
		  (atom ; its var or lambda-list-keyword, so...
		    (if(lambda-list-keyword-p elt)
		      (rec(cdr list)acc elt)
		      (rec(cdr list)(push elt acc)context)))
		  (list ; its init-form or nested lambda-list or method type specifier.
		    (if(null context) ; its nested lambda-list, or method, so...
		      (ecase as
			(:function (error "Destructuring is invalid for function."))
			(:method(rec(cdr list)(push(car elt)acc)context))
			(:macro(rec(cdr list)(append(vars<=lambda-list elt :as :macro)acc)context)))
		      (let((var(first elt))
			   (supplied-p(third elt)))
			(rec(cdr list)(if(listp var) ; its nested lambda-list.
					(if supplied-p
					  (append(vars<=lambda-list var)(list supplied-p)acc)
					  (append(vars<=lambda-list var)acc))
					(if supplied-p
					  (list* supplied-p var acc)
					  (push var acc)))
			  context)))))))))
    (rec(canonicalize-lambda-list lambda-list))))

(defun canonicalize-lambda-list(lambda-list &optional acc)
  #.(doc "doc/canonicalize-lambda-list.md")
  (if(null lambda-list)
    (nreverse acc)
    (if(atom lambda-list)
      (nreconc acc (list '&rest lambda-list))
      (if(listp(car lambda-list))
	(canonicalize-lambda-list(cdr lambda-list)
	  (cons(canonicalize-lambda-list(car lambda-list))acc))
	(canonicalize-lambda-list(cdr lambda-list)
	  (cons(car lambda-list)acc))))))

(defun lambda-list-keyword-p(symbol &optional(ansi t))
  #.(doc "doc/lambda-list-keyword-p.md")
  (if ansi
    (find symbol '(&optional &rest &key &body &allow-other-keys &aux &whole &environment):test #'eq)
    (char= #\&(char(symbol-name symbol)0))))
