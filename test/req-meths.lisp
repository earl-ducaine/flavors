;;; -*- Mode: LISP; Syntax: Common-lisp; Package: FLAVORS -*-
;;;
;;; ********************************************************************
;;;
;;;  (c) Copyright 1986 by Lucid Inc., All Rights Reserved
;;;
;;; ******************************************************************** 
;;; Edit-History: test file for :required-methods.  These tests need the
;;; test-stream-output macro loaded.
;;;
;;; Created: 12/29/85
;;; Reviewed: <Review-Date>
;;;
;;; 27-Jan-86 moe: removed (in-packge 'flavors) ,use (use-package 'flavors)
;;; 28-Jan-86 moe: don't try to import lucid:test nor  etest
;;; 31-oct-86 tomh attempts to instantiate flavors with missing
;;;                required- options now land in the debugger;
;;;                hence etests have been plugged in a few spots.
;;; 19-Mar-87 susan: modified so wouldn't run in application environment
;;;  8-Apr-87 tomh: fixed broken-eval stuff
;;; 22-Sep-87 lnz: Include (setf (symbol-plist ...) ...) within eval-when.
;;; 18-Nov-87 lnz: Removed format call.
;;;
;;; End-of-Edit-History

(in-package 'flavors)

(defvar *test-instance*)

(eval-when (eval compile load)

(setf (symbol-plist 'mixin) '())

(setf (symbol-plist 'other-mixin) '())

(setf (symbol-plist 'simple-test) '())

(setf (symbol-plist 'correct) '())

(setf (symbol-plist 'other-correct) '())

)

(defflavor simple-test () ()
  (:required-methods foo))

(defflavor mixin () ()
  )

(defmethod (mixin :foo) ()
  ())

(defflavor correct () (mixin)
  (:required-methods :foo))

(defflavor other-mixin () (mixin)
  )

(defflavor other-correct () (other-mixin)
  )

(etest (make-instance 'simple-test)
       "error-test for missing method in base flavor")
(test (make-instance 'correct)
      "flavor has required method in the mixin")
(test (make-instance 'other-correct)
      "flavor has required method in nested mixin")
