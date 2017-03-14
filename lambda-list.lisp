(in-package :cl-user)
(defpackage :lambda-list (:use :cl :type-ext :millet)
  (:export
    #:vars<=lambda-list
    #:canonicalize-lambda-list
    #:lambda-list-keyword-p
    ))
(in-package :lambda-list)

(define-simple-type(symbols (:element-type symbol)
			    (:element-predicate symbolp)))

(prototype vars<=lambda-list(list &key(:as(member :function :method :macro)))
	   symbols)
(defun vars<=lambda-list(lambda-list &key(as :function))
  (check-type lambda-list list)
  (labels((REC(list &optional acc context)
	    (if(endp list)
	      (nreverse acc)
	      (MAIN-DIVERGE (car list)(cdr list) acc context)))
	  (MAIN-DIVERGE(target rest acc context)
	    (if(atom target)
	      (ATOM-ON-TOP target rest acc context)
	      (LIST-ON-TOP context target rest acc)))
	  (ATOM-ON-TOP(var rest acc context)
	    (if(CHECK-VALIDITY(lambda-list-keyword-p var))
	      (REC rest acc var) ; don't accumulate it, but set as context.
	      (REC rest (cons var acc)context))) ; accumulate it.
	  (CHECK-VALIDITY(key)
	    (if(not(find key '(&environment &whole) :test #'eq))
	      key
	      (if(eq :macro as)
		key
		(error "Invalid lambda-list as ~S : ~S"as lambda-list))))
	  (LIST-ON-TOP(context sublis rest acc)
	    (if(null context) ; its nested lambda-list, or method, so...
	      (SUBLIS-WITHOUT-CONTEXT sublis rest acc)
	      (SUBLIS-WITHIN-CONTEXT sublis rest acc context)))
	  (SUBLIS-WITHOUT-CONTEXT(sublis rest acc)
	    (ecase as
	      (:function ; it's invalid form.
		(error "Invalid form~S~%Especially in ~S" lambda-list sublis))
	      (:method ; it's method class specifier.
		(REC rest(cons(car sublis)acc)))
	      (:macro ; it's nested lambda-list.
		(REC rest(nreconc(vars<=lambda-list sublis :as :macro)acc)))))
	  (SUBLIS-WITHIN-CONTEXT(sublis rest acc context)
	    (ecase context
	      (&key(&KEY sublis rest acc context))
	      (&optional(&OPTIONAL sublis rest acc context))
	      (&aux(&AUX sublis rest acc context))
	      ((&rest &allow-other-keys &body &whole &environment)
	       (error"Invalid form ~S~%Especially in ~S"lambda-list sublis))))
	  (&KEY(sublis rest acc context)
	    (let((var(first sublis)))
	      (if(listp var) ; it has alias.
		(%SUBLIS(second var)(third sublis)rest acc context)
		(%SUBLIS var (third sublis)rest acc context))))
	  (%SUBLIS(var predicate rest acc context)
	    (if predicate
	      (REC rest (list* predicate var acc)context)
	      (REC rest (cons var acc)context)))
	  (&OPTIONAL(sublis rest acc context)
	    (%SUBLIS(first sublis)(third sublis)rest acc context))
	  (&AUX(sublis rest acc context)
	    (%SUBLIS(car sublis)nil rest acc context)))
    (if(eq :macro as)
      (REC(canonicalize-lambda-list lambda-list))
      (REC lambda-list))))

(defun canonicalize-lambda-list(lambda-list &optional acc)
  (typecase lambda-list
    (NULL (nreverse acc))
    (ATOM (nreconc acc (list '&rest lambda-list)))
    (otherwise
      (canonicalize-lambda-list (cdr lambda-list)
				(cons (if(listp(car lambda-list))
					(canonicalize-lambda-list(car lambda-list))
					(car lambda-list))
				      acc)))))

(defun lambda-list-keyword-p(symbol &optional(ansi t))
  (if ansi
    (find symbol lambda-list-keywords :test #'eq)
    (and (char= #\&(char(symbol-name symbol)0))
	 symbol)))

(defun specialized-lambda-lists(symbol)
  (loop :for method :in (closer-mop:generic-function-methods(symbol-function symbol))
	:collect (cons symbol (specialized-lambda-list method))))

(defun specialized-lambda-list(method)
  (labels((REC(specializers lambda-lists &optional acc)
	    (if(endp specializers)
	      (nreconc acc lambda-lists)
	      (REC (cdr specializers)
		   (cdr lambda-lists)
		   (push `(,(car lambda-lists)
			    ,(SPECIALIZER(car specializers)))
			 acc))))
	  (SPECIALIZER(specializer)
	    (if(string= :eql-specializer(type-of specializer))
	      (list 'eql (closer-mop:eql-specializer-object specializer))
	      (class-name specializer))))
    (REC (closer-mop:method-specializers method)
	 (closer-mop:method-lambda-list method))))
