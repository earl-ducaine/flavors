;;; -*- Mode: LISP; Syntax: Common-lisp; Package: FLAVORS -*-
;;; 
;;;; vanilla, Module FLAVORS
;;;
;;; **********************************************************************
;;;
;;;   (c) Copyright 1986, 1987, 1988 by Lucid Inc., All Rights Reserved
;;;
;;; **********************************************************************
;;;
;;; Edit-History: test file for vanilla-flavor.  Requires test-print to be loaded
;;;
;;; Created: 1/3/86 moe
;;; Reviewed: <Review-Date>
;;;
;;; 27-Jan-86 moe: removed (in-packge 'flavors) ,use (use-package 'flavors)
;;; 28-Jan-86 moe: don't try to import lucid:test nor  etest
;;;  3-Apr-86 moe: add test for unclaimed-message with random message
;;;  6-Mar-87 maj: removed definition of set-equal (bug #01727)
;;; 10-Mar-87 maj: package qualified internal-init (buf #01737)
;;; 19-Mar-87 susan: modified for use in application environment
;;; 11-May-87 tomh: moved compile-flavor-methods stuff up 
;;; 18-Nov-87 lnz: Removed format call.
;;; 28-Dec-88 hardy: added title and module to header, updated copyright
;;;                  notice, cleaned up header
;;;
;;; End-of-Edit-History

(in-package sys:*testing-package-name*) 

(defvar *test-instance*)

(defvar *test-stream* (make-string-output-stream))

;(setf (symbol-plist 'mixin) ())

;(setf (symbol-plist 'flavor) ())

(defflavor mixin (miv1) ())

(defmethod (mixin :mixin-method) ()
  ())

(defflavor flavor (iv1 iv2 (iv3 0)) (mixin) )

(defmethod (flavor :unclaimed-message) (&rest ignore)
  'unclaimed-message)

(compile-flavor-methods mixin flavor)

(setq *test-instance* (make-instance 'flavor))

(defun vanilla-flavor-test1 ()
  (test (test-stream-output "#<Instance FLAVOR"
		    (send *test-instance* :print-self *test-stream* nil)
		    :print-stream *test-stream*)
		    "test :print-self")  
  (test (test-stream-output 
	  "An instance of flavor FLAVOR.
Instance variables:
IV1 unbound
IV2 unbound
IV3 0
MIV1 unbound" (send *test-instance* :describe)
	  :print-stream *standard-output*)
	"test :describe")
  
  (test (set-equal (send *test-instance* :which-operations)
	       '(:PRINT-SELF :DESCRIBE :SEND-IF-HANDLES TYPEP
			    :MIXIN-METHOD DESCRIBE :UNCLAIMED-MESSAGE
			    PRINT :WHICH-OPERATIONS
			    :OPERATION-HANDLED-P FLAVORS::INTERNAL-INIT))
	"test :which-operations"))

(defun vanilla-flavor-test2()
  
  (test (null (send *test-instance* :operation-handled-p :foo))
	"test :operation-handled-p for undefined message")
  
  (test (send *test-instance* :operation-handled-p :describe)
	"test :operation-handled-p for vanilla msg")
  
  (test (send *test-instance* :operation-handled-p :mixin-method)
	"test :operation-handled-p for mixin msg")
  
  (test (send *test-instance* :operation-handled-p :unclaimed-message)
	"test :operation-handled-p for flavor msg")
  
  (test (null (send *test-instance* :send-if-handles :foo))
	"test :send-if-handles for unhandled msg")
  
  (test (equal 'unclaimed-message
	       (send *test-instance* :send-if-handles :unclaimed-message))
	"test :send-if-handles for msg")
  
  (test (equal 'unclaimed-message
	       (send *test-instance* :unclaimed-message :foo))
	"test :unclaimed-message ")

  (test (equal 'unclaimed-message
	       (send *test-instance* :random-message))
	"test of unclaimed-message for random message")
  )

(vanilla-flavor-test1)
(vanilla-flavor-test2)
