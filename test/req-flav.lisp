;;; -*- Mode: LISP; Syntax: Common-lisp; Package: FLAVORS -*-
;;; 
;;; ********************************************************************
;;;
;;; (c) Copyright 1986 by Lucid Inc., All Rights Reserved
;;;
;;;*********************************************************************
;;;
;;; Edit-History: test file for :required-flavors.  These tests need the
;;; test-compiler-warnings macro loaded.
;;;
;;; Created: 12/29/85
;;; Reviewed: <Review-Date>
;;; 27-Jan-86 moe: removed (in-packge 'flavors) ,use (use-package 'flavors)
;;; 28-Jan-86 moe: don't try to import lucid:test nor  etest
;;; 31-oct-86 tomh attempts to instantiate flavors with missing
;;;                required- options now land in the debugger;
;;;                hence etests have been plugged in a few spots.
;;; 19-Mar-87 susan: modified for application environment...can't
;;;                  run since can't precompile with error in
;;;                  required methods
;;;  8-Apr-87 tomh: fixed broken-eval stuff
;;; 22-Sep-87 lnz: Include (setf (symbol-plist ...) ...) within eval-when.
;;; 18-Nov-87 lnz: Removed format call.
;;;
;;; End-of-Edit-History
 
(in-package 'flavors)

(eval-when (eval compile load)

(setf (symbol-plist 'mixin) '())

(setf (symbol-plist 'mixin2) '())

(setf (symbol-plist 'mixin3) '())

(setf (symbol-plist 'incorrect) '())

(setf (symbol-plist 'incorrect2) '())

(setf (symbol-plist 'correct) '())

(setf (symbol-plist 'correct2) '())

(setf (symbol-plist 'required-mixin) '())

)

(defflavor required-mixin () ())

(defflavor mixin () ())

(defflavor mixin2 () (required-mixin))

(defflavor mixin3 () ()
  (:required-flavors required-mixin))

(defflavor incorrect () (mixin)
  (:required-flavors required-mixin ))

(defflavor correct () (mixin mixin2)
  (:required-flavors required-mixin))

(defflavor incorrect2 () (mixin3)
  )

(defflavor correct2 () (mixin3 mixin2)
  )

(etest (make-instance 'incorrect) 
       "error test for instantiating a flavor without a required flavor")
(etest (make-instance 'incorrect2)  
       "second error test for instantiating a flavor without a required flavor") 
(test (make-instance 'correct)
      "Make sure that a flavor with required flavors is correctly instantiated")
(test (make-instance 'correct2)
      "Another test of instatiating flavors with required flavors")
