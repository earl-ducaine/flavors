;;; -*- Mode: LISP; Syntax: Common-lisp; Package: FLAVORS -*-
;;;  
;;; instancep.lisp, Module FLAVORS
;;;
;;; *********************************************************************
;;;
;;;  (c) Copyright 1986, 1987, 1988 by Lucid Inc., All Rights Reserved
;;;
;;; *********************************************************************
;;;
;;; Test file for instancep.
;;; Edit-History:
;;;
;;; Created: 2/18/86 by moe
;;;  6-May-87 moe: move call to make-instance to after compile-flavor-methods
;;;                call
;;; 23-Oct-87 jlm: let-binding for foo
;;; 24-Oct-87 jlm: removed let-binding, to avoid compile-time constant folding
;;;                foo => defvar *test-non-instance*
;;; 19-Nov-87 tomh: removed format statement 
;;;  3-Nov-88 hardy: added filename & Module to header, updated 
;;;                  copyright notice, cleaned up header
;;;
;;; End-of-Edit-History


(defvar *test-non-instance*)

(defflavor instancep-test ()())

(defun instancep-test ()
  (test (eq t (instancep *test-instance*))
	"test for instancep")
  (setq *test-non-instance* '(1 2 3))
  (test (null (instancep *test-non-instance*))
	"test for instancep returning nil"))

(compile-flavor-methods instancep-test)

(setq *test-instance* (make-instance 'instancep-test))

(instancep-test)

(fmakunbound 'instancep-test)

