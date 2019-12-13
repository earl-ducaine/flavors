;;; -*- Mode: LISP; Syntax: Common-lisp; Package: FLAVORS -*-
;;;
;;; incl-fl.lisp, Module FLAVORS
;;;
;;; ************************************************************************
;;;
;;;    (c) Copyright 1986, 1987, 1988 by Lucid Inc., All Rights Reserved
;;;
;;; ************************************************************************
;;;
;;; Edit-History: test file for :included-flavors
;;;
;;; Created: 12/30/85 moe
;;; Reviewed: <Review-Date>
;;; 27-Jan-86 moe: removed (in-packge 'flavors) ,use (use-package 'flavors)
;;; 28-Jan-86 moe: don't try to import lucid:test nor  etest
;;; 23-Jul-86 moe: add flavors:: to some non exported flavors functions
;;; 22-Sep-87 lnz: Include (setf (symbol-plist ...) ...) within eval-when.
;;; 18-Nov-87 lnz: Removed format call.
;;;  3-Nov-88 hardy: cleaned up Edit History, added filename & Module
;;;                  to header
;;;
;;; End-of-Edit-History

(use-package 'flavors)

(eval-when (eval compile load)

(setf (symbol-plist 'flavor1) '())

(setf (symbol-plist 'flavor2) '())

(setf (symbol-plist 'flavor3) '())

(setf (symbol-plist 'flavor4) '())

(setf (symbol-plist 'flavor5) '())

(setf (symbol-plist 'flavor6) '())

(setf (symbol-plist 'flavor7) '())

(setf (symbol-plist 'incl-flavor) '())

(setf (symbol-plist 'incl-flavor2) '())

(setf (symbol-plist 'simple) '())

(setf (symbol-plist 'complex1) '())

(setf (symbol-plist ' mother) '())

(setf (symbol-plist 'other) '())

)

(defflavor incl-flavor () ())

(defflavor incl-flavor2 () ()
  )

(defflavor simple () ()
	   (:included-flavors  incl-flavor incl-flavor2))

(defun get-list-of-flavor-names (list-of-flavors)
  (let ((name-list ()))
    (dolist (flavor list-of-flavors)
      (setq name-list (cons (flavors::flavor-name flavor) name-list )))
    (reverse name-list)))

(defflavor complex1 () ( simple)
	   (:included-flavors incl-flavor incl-flavor2))

(defflavor flavor4 () ())

(defflavor flavor5 () ())

(defflavor flavor2 () (flavor4 flavor5 )
	   )

(defflavor flavor3 () (flavor4))

(defflavor flavor1 () (flavor2 flavor3 )
	   (:included-flavors incl-flavor))

(defflavor flavor6 () (flavor4 flavor5)
  (:included-flavors incl-flavor))

(defflavor flavor7 () (flavor6 flavor3)
  (:included-flavors incl-flavor))

(defflavor other () (incl-flavor))

(defflavor mother () (other)
	   (:included-flavors incl-flavor))

(defun included-flavors-test ()

(test (equal '(simple incl-flavor incl-flavor2 vanilla-flavor)
	     (get-list-of-flavor-names
	       (flavors::flavor-all-components (flavors::get-flavor 'simple))))
      "test one flavor with includeds")
(test (equal '(COMPLEX1 SIMPLE INCL-FLAVOR INCL-FLAVOR2 VANILLA-FLAVOR)
	     (get-list-of-flavor-names
	       (flavors::flavor-all-components (flavors::get-flavor 'complex1))))
      "test flavor with includeds defined on both mixin and base flavor")
(test (equal '(FLAVOR1 INCL-FLAVOR FLAVOR2 FLAVOR4 FLAVOR5 FLAVOR3
		       VANILLA-FLAVOR)
	     (get-list-of-flavor-names (flavors::flavor-all-components (flavors::get-flavor
								'flavor1))))
      "test included-flavor with multiple mixins")
(test (equal '(flavor7 flavor6 incl-flavor flavor4 flavor5 flavor3
		       vanilla-flavor)
	     (get-list-of-flavor-names
	       (flavors::flavor-all-components (flavors::get-flavor 'flavor7))))
      "test included flavor with multiple included declarations")
(test (equal '(mother other incl-flavor vanilla-flavor)
	     (get-list-of-flavor-names
	       (flavors::flavor-all-components (flavors::get-flavor 'mother))))
      "test overriding included decls by explicit inclusion"))

(compile-flavor-methods  incl-flavor  incl-flavor2  simple
			 complex1  flavor4  flavor5  flavor2
			 flavor3  flavor1  flavor6  flavor7
			 other mother)

(included-flavors-test)
