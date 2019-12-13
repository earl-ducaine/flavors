;;; -*- Mode: LISP; Syntax: Common-lisp; Package: FLAVORS -*-
;;; 
;;; def-plist.lisp, Module FLAVORS
;;;
;;; ********************************************************************
;;;
;;;  (c) Copyright 1986, 1987, 1988 by Lucid Inc., All Rights Reserved
;;;
;;; ********************************************************************
;;;
;;; Edit-History:test file for defflavor option :default-init-plist.  It
;;; assumes that :gettable-instance-variables works.
;;;
;;; Created: <Creation-Date>
;;; Reviewed: <Review-Date>
;;;
;;; 27-Jan-86 moe: removed (in-packge 'flavors) ,use (use-package 'flavors)
;;; 28-Jan-86 moe: don't try to import lucid:test nor  etest
;;; 22-Sep-87 lnz: Include (setf (symbol-plist ...) ...) within eval-when.
;;; 18-Nov-87 lnz: Removed format call.
;;;  3-Nov-88 hardy: added filename & Module to header, updated 
;;;                  copyright notice
;;;
;;; End-of-Edit-History
 
(use-package 'flavors)

(defvar *test-instance*)

(eval-when (eval compile load)

(setf (symbol-plist 'mixin) '())

(setf (symbol-plist 'init-plist-test) '())

)

(defflavor mixin (miv1 miv2) ()
  :gettable-instance-variables
  :initable-instance-variables)

(defflavor init-plist-test (iv1 iv2) (mixin)
	   :initable-instance-variables 
	   :gettable-instance-variables
	   (:default-init-plist :iv1 1 :miv2 2 ))

(defun test-init-plist ()
  (setq *test-instance* (make-instance 'init-plist-test ))
  
  (test (equal 1   (send *test-instance* :iv1))
	"use default-init-plist to give default values
       to an initable instance var")
  
  (test (equal 2 (send *test-instance* :miv2))
	"use default-init-plist to give default value
       to a component flavor")
  
  (setq *test-instance* (make-instance 'init-plist-test :iv1 0 :miv2 0))
  
  (test (equal 0 (send *test-instance* :iv1))
	"override default-plist value")
  
  (test (equal 0 (send *test-instance* :miv2))
	"override default-plist value in mixin"))

(compile-flavor-methods mixin  init-plist-test)

(test-init-plist)
