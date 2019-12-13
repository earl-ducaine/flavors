;;; -*- Mode: LISP; Syntax: Common-lisp; Package: FLAVORS -*-
;;; 
;;; *********************************************************************
;;;
;;; (c) Copyright 1986 by Lucid Inc.,  All Rights Reserved
;;;
;;;**********************************************************************
;;;
;;; Edit-History:test file for defflavor option :required-instance-variables. 
;;; It requires that the test-compiler-warnings macro is loaded.
;;;
;;; Created: 12/28/85 moe
;;; Reviewed: <Review-Date>
;;;
;;; 27-Jan-86 moe: removed (in-packge 'flavors) ,use (use-package 'flavors)
;;; 28-Jan-86 moe: don't try to import lucid:test nor  etest
;;; 31-oct-86 tomh attempts to instantiate flavors with missing
;;;                required- options now land in the debugger;
;;;                hence etests have been plugged in a few spots.
;;; 19-Mar-87 susan: modified for use in application environment....
;;;                  can't run since breaks on purpose in required
;;;                  instance variables
;;;  8-Apr-87 tomh: fixed broken-eval stuff
;;; 22-Sep-87 lnz: Include (setf (symbol-plist ...) ...) within eval-when.
;;; 18-Nov-87 lnz: Removed format call.
;;;
;;; End-of-Edit-History
 
(in-package 'flavors)

(defvar *test-instance*)

(eval-when (eval compile load)

(setf (symbol-plist 'simple-required-test) '())

(setf (symbol-plist 'broken-required-test) '())

(setf (symbol-plist 'mixin) '())

(setf (symbol-plist 'required-test) '())

)

(defflavor simple-required-test (iv1 iv2) ()
  (:required-instance-variables iv1))

(defflavor broken-required-test ( iv2) ()
  (:required-instance-variables iv1))

(defflavor mixin (iv1) ()
  )

(defflavor required-test (iv1) (mixin) ;mixin provides the required iv
  (:required-instance-variables iv1))

(etest (make-instance 'broken-required-test)
       "error test for required instance variables")
(test (make-instance 'required-test)
      "make-instance with required iv supplied by mixin")





