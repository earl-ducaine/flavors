;;; -*- Mode: LISP; Syntax: Common-lisp; Package: FLAVORS -*-
;;;
;;; self.lisp, Module FLAVORS
;;;
;;; ************************************************************************
;;;
;;;    (c) Copyright 1986, 1987, 1988 by Lucid Inc.,  All Rights Reserved
;;;
;;; ************************************************************************
;;;
;;; Edit-History:test file for self variable.
;;;
;;; Created: by moe 3/4/86
;;; 19-Mar-87 susan: modified for use in application environment
;;;  6-May-87 moe: move call to make-instance to after compile-flavor-methods
;;; 18-Nov-87 lnz: Removed format call.
;;;  3-Nov-88 hardy: added filename & Module to header, updated 
;;;                  copyright notice, cleaned up header
;;;
;;; End-of-Edit-History


(defvar *test-instance*)

(defflavor self-test ()())

(defmethod (self-test :test) ()
  '(1 2 3))

(defmethod (self-test :test2) ()
  (send self :test))

(defmethod (self-test :test3) ()
  '(4 5 6))

(defmethod (self-test :test4) ()
  (cons (send self :test) (send self :test3)))


(defun self-test ()
      (test (equal '(1 2 3) (send *test-instance* :test))
	    "self test simple method")
      (test (equal '(1 2 3) (send *test-instance* :test2))
	    "self test by method indirection")
      (test (equal '((1 2 3) 4 5 6) (send *test-instance* :test4))
	    "self test by multiple indirection"))

(compile-flavor-methods self-test)

(setq *test-instance* (make-instance 'self-test))

(self-test)

(fmakunbound 'self-test)
