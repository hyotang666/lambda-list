(defpackage :lambda-list.spec (:use :cl :jingoh :lambda-list))
(in-package :lambda-list.spec)
(setup :lambda-list)

(requirements-about VARS<=LAMBDA-LIST)

;;;; Description:
; accepts lambda-list, return vars.
#?(vars<=lambda-list '(a b c))
=> (a b c)
,:test equal
#?(vars<=lambda-list '(a &rest b))
=> (a b)
,:test equal
#?(vars<=lambda-list '(a &optional(b :hoge supplied-p)))
=> (a b supplied-p)
,:test equal
#?(vars<=lambda-list '(a &key (b :hoge supplied-p)))
=> (a b supplied-p)
,:test equal
#?(vars<=lambda-list '(a &key((:stream *standard-output*)*standard-output*)))
=> (a *standard-output*)
,:test equal
#?(vars<=lambda-list '(a &aux (b a)))
=> (a b)
,:test equal

#+syntax
(VARS<=LAMBDA-LIST lambda-list &key (as :function)) ; => result

;;;; Arguments and Values:

; lambda-list := lambda-list, otherwise error.
#?(vars<=lambda-list "not-lambda-list") :signals error
,:ignore-signals warning

; as := (member :function :macro :method)
#?(vars<=lambda-list '((a symbol))) :signals lambda-list-simple-error
#?(vars<=lambda-list '((a symbol)) :as :method)
=> (a)
,:test equal
#?(vars<=lambda-list '((a symbol)) :as :macro)
=> (a symbol)
,:test equal
#?(vars<=lambda-list '(a . b)) :signals lambda-list-simple-error
#?(vars<=lambda-list '(a . b) :as :method) :signals lambda-list-simple-error
#?(vars<=lambda-list '(a . b) :as :macro)
=> (a b)
,:test equal
#?(vars<=lambda-list '(&whole whole a b)) :signals lambda-list-simple-error
#?(vars<=lambda-list '(&whole whole a b) :as :method) :signals lambda-list-simple-error
#?(vars<=lambda-list '(&whole whole a b) :as :macro)
=> (whole a b)
,:test equal
#?(vars<=lambda-list '(a &environment env)) :signals lambda-list-simple-error
#?(vars<=lambda-list '(a &environment env) :as :method) :signals lambda-list-simple-error
#?(vars<=lambda-list '(a &environment env) :as :macro)
=> (a env)
,:test equal

; result := list which includes symbols.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about CANONICALIZE-LAMBDA-LIST)

;;;; Description:
; If lambda-list is dotted list, canonicalize to (... &rest var).
#?(canonicalize-lambda-list '(a . b))
=> (A &REST B)
,:test equal

#+syntax
(CANONICALIZE-LAMBDA-LIST lambda-list &optional acc) ; => result

;;;; Arguments and Values:

; lambda-list := macro lambda list, otherwise unspecifie.
#?(canonicalize-lambda-list "not lambda-list")
=> unspecified

; acc := list ; internal use.

; result := macro lambda-list

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:
; Nested lambda list also acceptable.
#?(canonicalize-lambda-list '(a (b . c) d . e))
=> (A (B &REST C) D &REST E)
,:test equal

;;;; Exceptional-Situations:

(requirements-about LAMBDA-LIST-KEYWORD-P)

;;;; Description:
; Tests arg is lambda-list-keyword or not.
#?(lambda-list-keyword-p '&rest) => &REST
; otherwise nil.
#?(lambda-list-keyword-p "not") => NIL

#+syntax
(LAMBDA-LIST-KEYWORD-P symbol &optional (ansi t)) ; => result

;;;; Arguments and Values:

; symbol := any lisp object

; ansi := boolean which specify algorithm.
; If specified T, tests symbol is found in CL:LAMBDA-LIST-KEYWORDS.
; otherwise, tests symbol-name prefixed "&".
; This is useful when you want to extend lambda-list.
; You can find such example in esrap.
#?(lambda-list-keyword-p '&hoge) => NIL
#?(lambda-list-keyword-p '&hoge nil) => &HOGE

; result := boolean

;;;; Affected By:
; CL:LAMBDA-LIST-KEYWORD state.
; It is constant, but implementations are allowed to customize it value by ansi-cl standard.

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

