;;; -*- Mode: LISP; Syntax: Common-lisp; Package: FLAVORS -*-
;;;
;;; **********************************************************************
;;;
;;; (c) Copyright 1986 by Lucid Inc., All Rights Reserved
;;;
;;; ********************************************************************
;;;
;;; File defmethod.lisp tests the function defmethod and undefmethod.
;;; Method combinations will be tested in another file.
;;; Edit-History:
;;;
;;; Created: 11/3/85 by moe
;;; Reviewed: <Review-Date>
;;; 27-Jan-86 moe: removed (in-packge 'flavors) ,use (use-package 'flavors)
;;; 28-Jan-86 moe: don't try to import lucid:test nor  etest
;;; 19-Nov-86 lnz: flavors-system ==> flavor-internals.
;;; 19-Mar-87 susan: made runnable in application environment
;;; 11-May-87 tomh: updated type-of stuff
;;; 22-Sep-87 lnz: Include (setf (symbol-plist ...) ...) within eval-when.
;;;
;;; End-of-Edit-History

(in-package sys:*testing-package-name*)

(eval-when (eval compile load)

(setf (symbol-plist 'mixin) '())

(setf (symbol-plist 'base-flavor) '())

(setf (symbol-plist 'lowest-mixin) '())

(setf (symbol-plist 'middle-mixin) '())

(setf (symbol-plist 'top-flavor) '())

(setf (symbol-plist 'multiple-mixins) '())

)

(defvar *test-instance*)

(defflavor mixin ((mixin-iv "mixin-iv") (mixin-iv-2 "mixin-iv-2")) ()
	   )

(defflavor base-flavor ((base-iv "base-iv")) (mixin)
	   )

(defmethod (base-flavor :make-list) ()
  (list mixin-iv mixin-iv-2 base-iv))


						;stacked mixin test
(defflavor lowest-mixin ((lowest-mixin-iv "lowest-mixin-iv")) ()
	   )

(defflavor middle-mixin ((middle-mixin-iv "middle-mixin-iv"))
  (lowest-mixin)
	   )

(defflavor top-flavor ((top-flavor-iv "top-flavor-iv"))
  (middle-mixin)
	   )

(defmethod (top-flavor :make-list) ()
  (list lowest-mixin-iv middle-mixin-iv top-flavor-iv))


						;multiple mixin test
(defflavor multiple-mixins ((multiple-mixin-iv "multiple-mixin-iv"))
  (top-flavor base-flavor)
	   )
 
(defmethod (multiple-mixins :make-list) ()
  (list lowest-mixin-iv middle-mixin-iv top-flavor-iv mixin-iv base-iv))

(compile-flavor-methods  mixin   base-flavor  lowest-mixin
			 middle-mixin top-flavor multiple-mixins)


  (test (eq 'multiple-mixins
	 (type-of (setq *test-instance* (make-instance 'multiple-mixins))))
     "make an instance of base with multiple mixins")
  (test (equal '("lowest-mixin-iv" "middle-mixin-iv"
		 "top-flavor-iv" "mixin-iv" "base-iv")
	       (send *test-instance* :make-list))
	"test simple method with multiple mixins")

(protect-from-compilation 
  (undefmethod (multiple-mixins :make-list))	;should use top-flavor's method
  (compile-flavor-methods multiple-mixins)
  (test (equal '("lowest-mixin-iv" "middle-mixin-iv" "top-flavor-iv")
	       (send *test-instance* :make-list)) "remove primary method")
  (undefmethod (top-flavor :make-list))		;should use base-flavor's method
  (compile-flavor-methods multiple-mixins)
  (test (equal '("mixin-iv"  "mixin-iv-2" "base-iv")
	       (send *test-instance* :make-list))
	"remove component method")
  (undefmethod (base-flavor :make-list))	;no handler's left
  (compile-flavor-methods multiple-mixins)
  (etest (send *test-instance* :make-list) "send msg to undefmethoded method"))
