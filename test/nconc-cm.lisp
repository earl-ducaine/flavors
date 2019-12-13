;;; -*- Mode: LISP; Syntax: Common-lisp; Package: FLAVORS -*-
;;;
;;; nconc-cm.lisp, Module FLAVORS
;;;
;;; ***********************************************************************
;;; 
;;;   (c) Copyright 1986, 1987, 1988 by Lucid Inc.,  All Rights Reserved
;;;
;;; ***********************************************************************
;;;
;;; Test file for :nconc method combination.
;;;
;;; Edit-History:
;;;
;;; Created: 11/12/85 by moe
;;; Reviewed: <Review-Date>
;;;
;;; 27-Jan-86 moe: removed (in-packge 'flavors) ,use (use-package 'flavors)
;;; 28-Jan-86 moe: don't try to import lucid:test nor  etest
;;; 19-Mar-87 susan: updated for running in an application environment
;;; 22-Sep-87 lnz: Include (setf (symbol-plist ...) ...) within eval-when.
;;; 18-Nov-87 lnz: Removed format call.
;;;  3-Nov-88 hardy: added filename & Module to header, updated 
;;;                  copyright notice
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

(defmethod (flavor-1 :make-lists) ()
  (list flavor-1-iv   ))

(defmethod (flavor-1 :make-lists-2) ()
  (list flavor-1-iv ))

(defmethod (flavor-1 :make-lists-3) ()
  ())

(defmethod (flavor-1 :make-lists-4) ()
  *test-list*)

(defflavor flavor-3  ((flavor-3-iv "flavor-3-iv") )
	   ())

(defmethod (flavor-3  :make-lists) ()
  (list  flavor-3-iv ))
  

(defmethod (flavor-3  :make-lists-2) ()
  (list  flavor-3-iv ))  

(defmethod (flavor-3  :make-lists-3) ()
   (list flavor-3-iv ))

(defmethod (flavor-3  :make-lists-4) ()
   (list flavor-3-iv )) 

(defflavor flavor-2 ((flavor-2-iv "flavor-2-iv") ) (flavor-1 flavor-3)
    :gettable-instance-variables	   )

(defmethod (flavor-2  :make-lists) ()
  (list  flavor-2-iv ))

(defmethod (flavor-2  :make-lists-2) ()
  (list flavor-2-iv ))

(defmethod (flavor-2  :make-lists-3) ()
  (list  flavor-2-iv ))

(defmethod (flavor-2  :make-lists-4) ()
  (list  flavor-2-iv ))

(defflavor flavor-4 ((flavor-4-iv "flavor-4-iv") )
  (flavor-2) 
  (:method-combination (:nconc :base-flavor-first :make-lists)
                       (:nconc :base-flavor-last :make-lists-2)
		       (:nconc :base-flavor-first :make-lists-3)
		       (:nconc :base-flavor-last :make-lists-4))
  :gettable-instance-variables)

(defmethod (flavor-4  :make-lists) ()	
  (list  flavor-4-iv ))

(defmethod (flavor-4  :make-lists-2) ()	
  (list  flavor-4-iv ))

(defmethod (flavor-4  :make-lists-3) ()	
  (list  flavor-4-iv ))

(defmethod (flavor-4  :make-lists-4) ()	
  (list  flavor-4-iv ))

(defun multiple-nconc-test ()
  (setq *test-instance* (make-instance 'flavor-4))
  (test (equal '("flavor-3-iv" "flavor-1-iv" "flavor-2-iv" "flavor-4-iv")
               (send *test-instance* :make-lists))
	":nconc :base-flavor-first test")
  (test (equal '("flavor-4-iv" "flavor-2-iv" "flavor-1-iv" "flavor-3-iv")
	       (send *test-instance* :make-lists-2))
	":nconc :base-flavor-last test")
  (test  (equal '("flavor-3-iv" "flavor-2-iv" "flavor-4-iv")
		(send *test-instance* :make-lists-3))
	"method returns a list, :base-flavor-first")
   (setq *test-list* ())
  (test (equal '("flavor-4-iv" "flavor-2-iv" "flavor-3-iv")
	       (send *test-instance* :make-lists-4))
	"method returns a list, :base-flavor-last"))

(compile-flavor-methods flavor-1 flavor-3 flavor-2 flavor-4)

(multiple-nconc-test)
