;;; -*- Mode: LISP; Syntax: Common-lisp; Package: FLAVORS -*-
;;; 
;;; init-keys.lisp, Module FLAVORS
;;;
;;; **********************************************************************
;;;
;;;   (c) Copyright 1986, 1987, 1988 by Lucid Inc., All Rights Reserved
;;;
;;; **********************************************************************
;;;
;;; Edit-History:
;;;
;;; Created: <Creation-Date>
;;; Reviewed: <Review-Date>
;;; 27-Jan-86 moe: removed (in-packge 'flavors) ,use (use-package 'flavors)
;;; 28-Jan-86 moe: don't try to import lucid:test nor  etest
;;; 22-Sep-87 lnz: Include (setf (symbol-plist ...) ...) within eval-when.
;;;  5-Oct-87 moe: add test for bug-2302 (extra evaluation of init-keywords)
;;; 18-Nov-87 lnz: Removed format call.
;;;  3-Nov-88 hardy: added filename & Module to header, updated
;;;                  copyright notice, cleaned up header
;;;
;;; End-of-Edit-History
 
(use-package 'flavors)

(defvar *test-instance*)

;;;make sure we are starting with previously undefined flavors

(eval-when (eval compile load)

(setf (symbol-plist 'mixin) '())

(setf (symbol-plist 'key-test) '())

(setf (symbol-plist 'simple-key-test) '())

(setf (symbol-plist 'test) nil)
)

(defflavor simple-key-test (iv1 iv2) ()
	   (:initable-instance-variables iv1)
	   (:init-keywords :iv2) )


(defflavor mixin (miv1 miv2) ()
	   :initable-instance-variables
	   (:init-keywords :mixin-init-key)
	   )

(defflavor key-test (iv1 iv2) (mixin)
	   :initable-instance-variables
	   (:init-keywords :init-key)
	   )

(defflavor test () () (:init-keywords :keyword))

(defun init-key-test ()
  (test (make-instance 'test :keyword '(1 2 3 4)) "bug 2302 - init-keywords ~
						evaluated correctly")
  (test (make-instance 'simple-key-test :iv1 0 :iv2 0 )
	"make-instance using init-keywords")
  
  (etest (make-instance 'simple-key-test :iv1 0 :ivv2 0)
	 "make-instance using bogus keywords")
  
  (test (equal '(:iv1 :iv2) (flavor-allowed-init-keywords 'simple-key-test))
	"flavor-allowed-init-keywords on simple-key-test")
  (test  (make-instance 'key-test :iv1 0
			:iv2 0 :miv1 0 :miv2 0 :init-key 0
			:mixin-init-key 0)
	 "make-instance with mixin using init-keys")
  
  (etest (make-instance 'key-test :mmiv 0)
	 "etest for bogus keywords")
  
  (test (equal '(:INIT-KEY :IV1 :IV2 :MIV1 :MIV2 :MIXIN-INIT-KEY)
	       (flavor-allowed-init-keywords 'key-test))
	"flavor-allowed-init-keywords on key-test"))

(compile-flavor-methods   simple-key-test  mixin  key-test)

(init-key-test)
