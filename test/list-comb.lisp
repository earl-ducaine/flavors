;;; -*- Mode: LISP; Syntax: Common-lisp; Package: FLAVORS -*-
;;;
;;; list-comb.lisp, Module FLAVORS
;;;
;;; ***********************************************************************
;;;
;;;   (c) Copyright 1986, 1987, 1988 by Lucid Inc.,  All Rights Reserved
;;;
;;; ***********************************************************************
;;;
;;; Test file for :list method combination.
;;;
;;; Edit-History:
;;;
;;; Created: 11/11/85 by moe
;;; Reviewed: <Review-Date>
;;; 27-Jan-86 moe: removed (in-packge 'flavors) ,use (use-package 'flavors)
;;; 28-Jan-86 moe: don't try to import lucid:test nor  etest
;;; 19-Mar-87 susan: updated to run in application environment
;;; 22-Sep-87 lnz: Include (setf (symbol-plist ...) ...) within eval-when.
;;; 18-Nov-87 lnz: Removed format call.
;;;  3-Nov-88 hardy: added filename & Module to header, updated 
;;;                  copyright notice, cleaned up heade
;;;
;;; End-of-Edit-History

(use-package 'flavors)

(eval-when (eval compile load)

(setf (symbol-plist 'flavor-1) '())

(setf (symbol-plist 'flavor-2) '())

(setf (symbol-plist 'flavor-3) '())

(setf (symbol-plist 'flavor-4) '())

)

(defvar *test-instance*)

(defvar *test-list* '())

;;; test with multiple mixins

(defflavor flavor-1 ((flavor-1-iv "flavor-1-iv")) ()
  :gettable-instance-variables)

(defmethod (flavor-1 :get-iv) ()
  flavor-1-iv)

(defmethod (flavor-1 :get-iv-2) ()
  flavor-1-iv)

(defflavor flavor-3  ((flavor-3-iv "flavor-3-iv") )
	   ())

(defmethod (flavor-3  :get-iv) ()
  flavor-3-iv
  )

(defmethod (flavor-3  :get-iv-2) ()
  flavor-3-iv
  )

(defflavor flavor-2 ((flavor-2-iv "flavor-2-iv") ) (flavor-1 flavor-3)
   :gettable-instance-variables	   )

(defmethod (flavor-2  :get-iv) ()
  flavor-2-iv
  )

(defmethod (flavor-2  :get-iv-2) ()
  flavor-2-iv
  )

(defflavor flavor-4 ((flavor-4-iv "flavor-4-iv") )
  (flavor-2) 
  (:method-combination (:list :base-flavor-first :get-iv)
                       (:list :base-flavor-last :get-iv-2))
  :gettable-instance-variables)

(defmethod (flavor-4  :get-iv) ()	
  flavor-4-iv)

(defmethod (flavor-4  :get-iv-2) ()	
  flavor-4-iv)

(defun list-test ()
  (setq *test-instance* (make-instance 'flavor-4))
  (test (equal '("flavor-3-iv" "flavor-1-iv" "flavor-2-iv" "flavor-4-iv")
                (send *test-instance* :get-iv))
	":list :base-flavor-first test")	
  (test (equal '("flavor-4-iv" "flavor-2-iv" "flavor-1-iv" "flavor-3-iv")
	       (send *test-instance* :get-iv-2))
	":list :base-flavor-last test"))	

(compile-flavor-methods flavor-1 flavor-3 flavor-2 flavor-4)

(list-test)
