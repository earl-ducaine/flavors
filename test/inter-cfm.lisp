;;; -*- Mode: LISP; Syntax: Common-lisp; Package: FLAVORS -*-
;;;
;;; *************************************************************************
;;;
;;; (c) Copyright 1986 by Lucid Inc., All Rights Reserved
;;;
;;; **************************************************************************
;;;
;;; Edit-History:
;;;
;;; Created: 1/29/86 After calling compile-flavor, the
;;;                   flavor structure has an entry for flavors::description
;;;                   which in turn contains the message hash table.
;;; 19-Mar-87 susan: protected from running in application environment
;;;  8-Apr-87 tomh: fixed broken eval stuff 
;;; 22-Sep-87 lnz: Include (setf (symbol-plist ...) ...) within eval-when.
;;; 31-Oct-87 tomh: let binding for flavor-obj
;;; 19-Nov-87 tomh: removed format statement 
;;; Reviewed: <Review-Date>
;;; End-of-Edit-History

(in-package 'flavors)

(eval-when (eval compile load)

(setf (symbol-plist 'test-flavor) '())
  
(setf (symbol-plist 'test-flavor-2) '()) ;make sure that flavor was
					;never defined
)

(defflavor test-flavor ((test-flavor-iv "test-flavor-iv")) ()	;simple flavor
	   )
(defflavor test-flavor-2 ((test-flavor-2-iv "test-flavor-2-iv"))
  (test-flavor)			;flavor with mixin
  )
(defmethod (test-flavor :method) ()	;simple method tests
  ())
(defmethod (test-flavor-2 :method) ()	;simple method tests
  ())
(defflavor test-flavor-3 ()())

(let ((flavor-obj nil))
  (setq flavor-obj (flavors::get-flavor 'test-flavor))
  (test (null (flavors::flavor-descriptor flavor-obj))
	"before hash table generation")
  (compile-flavor-methods test-flavor)
  (test (arrayp (flavors::flavor-descriptor flavor-obj))
	"after hash table generation")
  (setq flavor-obj (flavors::get-flavor 'test-flavor-2))
  (test (null (flavors::flavor-descriptor flavor-obj))
	"before hash table generation, flavor with mixin")
  (compile-flavor-methods test-flavor-2 test-flavor-3)
  (test (arrayp (flavors::flavor-descriptor flavor-obj))
	"after hash table generation,flavor with mixin")
  (setq flavor-obj (flavors::get-flavor 'test-flavor-3))
  (test (arrayp (flavors::flavor-descriptor flavor-obj))
	"after hash table generation, simple flavor"))
