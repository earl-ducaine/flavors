;;; -*- Mode: LISP; Syntax: Common-lisp; Package: FLAVORS -*-
;;;
;;; no-vanill.lisp, Module FLAVORS
;;;
;;; ***********************************************************************
;;;
;;;   (c) Copyright 1986, 1987, 1988 by Lucid Inc.,  All Rights Reserved
;;;
;;; ***********************************************************************
;;;
;;; Edit-History: test file for :no-vanilla-flavor
;;;
;;; Created: 1/3/86 moe
;;; Reviewed: <Review-Date>
;;;
;;; 27-Jan-86 moe: removed (in-packge 'flavors) ,use (use-package 'flavors)
;;; 28-Jan-86 moe: don't try to import lucid:test nor  etest
;;; 23-Jul-86 moe: add flavors:: to some non exported flavors functions
;;; 19-Mar-87 susan: modified for running in application environment
;;; 22-Sep-87 lnz: Include (setf (symbol-plist ...) ...) within eval-when.
;;; 18-Nov-87 lnz: Removed format call.
;;;  3-Nov-88 hardy: added filename & Module to header, updated 
;;;                  copyright notice, cleaned up header
;;;
;;; End-of-Edit-History

(use-package 'flavors)

(eval-when (eval compile load)

(setf (symbol-plist 'flavor1) '())

(setf (symbol-plist 'flavor2) '())

(setf (symbol-plist 'flavor3) '())

(setf (symbol-plist 'flavor4) '())

(setf (symbol-plist 'flavor5) '())

)

(defun get-list-of-flavor-names (list-of-flavors)
  (let ((name-list ()))
    (dolist (flavor list-of-flavors)
      (setq name-list (cons (flavors::flavor-name flavor) name-list )))
    (reverse name-list)))

(defflavor flavor1 () ()
	   :no-vanilla-flavor)

(defflavor flavor2 () (flavor1)
	   )

(defflavor flavor3 () (flavor2) )

(defflavor flavor4 () ()
	   )

(defflavor flavor5 () (flavor4 flavor3) )

(defun no-vanilla-flavor-test ()
  (test (equal '(flavor1)
	       (get-list-of-flavor-names
		 (flavors::flavor-all-components
		  (flavors::get-flavor 'flavor1))))
	"test one flavor, no-vanilla-flavor")
  
  (test (equal '(flavor2 flavor1)
	       (get-list-of-flavor-names
		 (flavors::flavor-all-components
		  (flavors::get-flavor 'flavor2))))
	"test component flavor declares no-vanilla-flavor")
  
  (test (equal '(flavor5 flavor4 flavor3 flavor2 flavor1)
	       (get-list-of-flavor-names
		 (flavors::flavor-all-components
		  (flavors::get-flavor 'flavor5))))
	"test deeply nested component flavor declares no-vanilla-flavor"))

(compile-flavor-methods flavor1 flavor2 flavor3 flavor4 flavor5)

(no-vanilla-flavor-test)
