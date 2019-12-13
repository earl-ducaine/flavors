;;; -*- Mode: LISP; Syntax: Common-lisp; Package: FLAVORS -*-
;;;  
;;; *************************************************************************
;;; 
;;;    (c) Copyright 1986, 1987, 1988 by Lucid Inc., All Rights Reserved
;;;
;;; *************************************************************************
;;;
;;; Test file for defflavor function.  Tests defflavor with instance variables,
;;; component flavors, but not options to defflavor.
;;;
;;; Edit-History:
;;;
;;; Created: 10/31/85 by moe
;;; Reviewed: <Review-Date>
;;;
;;; 27-Jan-86 moe: removed (in-packge 'flavors) ,use (use-package 'flavors)
;;; 28-Jan-86 moe: don't try to import lucid:test nor  etest
;;; 19-Nov-86 lnz: flavors-system ==> flavor-internals.
;;; 11-May-87 tomh: fixed type-of stuff
;;;  2-Jul-87 moe: fix bug in all-flavors-test
;;; 22-Sep-87 lnz: Include (setf (symbol-plist ...) ...) within eval-when.
;;; 18-Nov-87 lnz: Removed format call.
;;; 21-Nov-88 hardy: updated copyright notice, added test for bug #2855
;;;
;;; End-of-Edit-History

(in-package sys:*testing-package-name*)

(eval-when (eval compile load)

(setf (symbol-plist 'test-flavor) '())

(setf (symbol-plist 'test-flavor-multiple-iv) '())

(setf (symbol-plist 'base-flavor) '())

(setf (symbol-plist 'lowest-mixin) '())

(setf (symbol-plist 'middle-mixin) '())

(setf (symbol-plist 'top-flavor) '())

(setf (symbol-plist 'multiple-mixins) '())

(setf (symbol-plist 'mixin0) '())

)

(defvar *test-instance*)

(defflavor test-flavor ((test-iv "test-iv")) ()	;simple flavor
	   )

(defmethod (test-flavor :make-list) ()		;simple method tests
   (list test-iv))		                ;access to own ivs



						;multiple iv's
(defflavor test-flavor-multiple-iv ((test-iv "test-iv")
				    (test-iv-2 "test-iv-2")
				    (test-iv-3 "test-iv-3"))()
	   )

(defmethod (test-flavor-multiple-iv :make-list) ()
  (list test-iv test-iv-2 test-iv-3))

						;simple mixin test

(defflavor mixin0 ((mixin-iv "mixin-iv") (mixin-iv-2 "mixin-iv-2")) ()
	   )

(defflavor base-flavor ((base-iv "base-iv")) (mixin0)
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


(defun simple-defflavor-test ()
   (test (eq 'test-flavor
	 (type-of (setq *test-instance*
			(make-instance 'test-flavor))))
     "make an instance")
  (test (equal '("test-iv") (send *test-instance* :make-list))
    "test simple method")
  (etest (send *test-instance* :test-iv)
	 "etest bogus message")
  )

(defun multiple-iv-test ()
   (test
     (eq 'test-flavor-multiple-iv
	 (type-of (setq *test-instance*
			(make-instance 'test-flavor-multiple-iv))))
     "make an instance of multiple iv")
  (test (equal '("test-iv" "test-iv-2" "test-iv-3")
	       (send *test-instance* :make-list))
	"test simple method multiple iv")
  (etest (send *test-instance* :test-iv-3)
	 "etest bogus message"))

(defun simple-mixin-test ()
  (test
     (eq 'base-flavor
	 (type-of (setq *test-instance* (make-instance 'base-flavor))))
     "make an instance of base with mixin")
  (test (equal '("mixin-iv" "mixin-iv-2" "base-iv")
	       (send *test-instance* :make-list))
	"test simple method with mixin")
  (etest (send *test-instance* :test-iv-3)
	 "etest bogus message"))

(defun stacked-mixin-test ()
  (test
     (eq 'top-flavor
	 (type-of (setq *test-instance* (make-instance 'top-flavor))))
     "make an instance of base with stacked mixins")
  (test (equal '("lowest-mixin-iv" "middle-mixin-iv" "top-flavor-iv")
	       (send *test-instance* :make-list))
	"test simple method with stacked mixins")
  (etest (send *test-instance* :test-iv-3)
	 "etest bogus message"))

(defun multiple-mixins-test ()
  (test
     (eq 'multiple-mixins
	 (type-of (setq *test-instance* (make-instance 'multiple-mixins))))
     "make an instance of base with multiple mixins")
  (test (equal '("lowest-mixin-iv" "middle-mixin-iv"
		 "top-flavor-iv" "mixin-iv" "base-iv")
	       (send *test-instance* :make-list))
	"test simple method with multiple mixins")
  (etest (send *test-instance* :test-iv-3)
	 "etest bogus message"))

(defun *all-flavor-names*-test-aux (flavor-list)
  (let ((pass-test-p t))
    (do ((fl-list flavor-list (cdr fl-list)))
	((null fl-list) pass-test-p)
      (setq pass-test-p
	    (and pass-test-p (member (car fl-list) *all-flavor-names*))))))

(defun *all-flavor-names*-test()
  (test (not (null (*all-flavor-names*-test-aux'(MULTIPLE-MIXINS TOP-FLAVOR MIDDLE-MIXIN
					LOWEST-MIXIN BASE-FLAVOR MIXIN0
					TEST-FLAVOR-MULTIPLE-IV 
					TEST-FLAVOR VANILLA-FLAVOR)
		     ))) "*all-flavor-names* test"))



(defun defflavor-test ()
  (simple-defflavor-test )
  (multiple-iv-test )
  (simple-mixin-test )
  (stacked-mixin-test )
  (multiple-mixins-test )
  (*all-flavor-names*-test))


(compile-flavor-methods  test-flavor  test-flavor-multiple-iv
			  mixin0  base-flavor
			   lowest-mixin  middle-mixin 
			    top-flavor  multiple-mixins)

(defflavor-test)

(defflavor a ((x 1) y) () :settable-instance-variables)
(defflavor b (z) (a) :settable-instance-variables)
(setq test-instance (make-instance 'b :x 5))
(test (equal 5 (send test-instance :x))) ;bug #2855

