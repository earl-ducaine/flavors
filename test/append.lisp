;;; -*- Mode: LISP; Syntax: Common-lisp; Package: FLAVORS -*-
;;;
;;; append.lisp, Module FLAVORS
;;;
;;; ********************************************************************
;;;
;;;  (c) Copyright 1986, 1987, 1988 by Lucid Inc., All Rights Reserved
;;;
;;; ********************************************************************
;;; Test file for :append method combination.
;;;
;;; Edit-History:
;;;
;;; Created: 11/11/85 by moe
;;; Reviewed: <Review-Date>
;;;
;;; 27-Jan-86 moe: removed (in-packge 'flavors) ,use (use-package 'flavors)
;;; 28-Jan-86 moe: don't try to import lucid:test nor  etest
;;; 22-Sep-87 lnz: Include (setf (symbol-plist ...) ...) within eval-when.
;;; 18-Nov-87 lnz: Removed format call.
;;;  3-Nov-88 hardy: Added filename into header, updated copyright
;;;                  notice, added Module FLAVORS.
;;;
;;; End-of-Edit-History

(use-package 'flavors)

(defvar *test-instance*)

(defvar *test-list* '())

(eval-when (compile eval load)

(setf (symbol-plist 'flavor-1) '())

(setf (symbol-plist 'flavor-2) '())

(setf (symbol-plist 'flavor-3) '())

(setf (symbol-plist 'flavor-4) '())

)

;;; test with multiple mixins

(defflavor flavor-1 ((flavor-1-iv "flavor-1-iv")) ()
  :gettable-instance-variables)

(defmethod (flavor-1 :make-lists) ()
  (list flavor-1-iv   ))

(defmethod (flavor-1 :make-lists-2) ()
  (list flavor-1-iv ))

(defmethod (flavor-1 :make-lists-3) ()
  'thing)

(defflavor flavor-3  ((flavor-3-iv "flavor-3-iv") )
	   ())

(defmethod (flavor-3  :make-lists) ()
  (list  flavor-3-iv ))
  

(defmethod (flavor-3  :make-lists-2) ()
  (list  flavor-3-iv ))  

(defmethod (flavor-3  :make-lists-3) ()
   (list flavor-3-iv )) 

(defflavor flavor-2 ((flavor-2-iv "flavor-2-iv") ) (flavor-1 flavor-3)
   :gettable-instance-variables	   )

(defmethod (flavor-2  :make-lists) ()
  (list  flavor-2-iv ))

(defmethod (flavor-2  :make-lists-2) ()
  (list flavor-2-iv ))

(defmethod (flavor-2  :make-lists-3) ()
  (list  flavor-2-iv ))

(defflavor flavor-4 ((flavor-4-iv "flavor-4-iv") )
  (flavor-2) 
  (:method-combination (:append :base-flavor-first :make-lists)
                       (:append :base-flavor-last :make-lists-2)
		       (:append :base-flavor-first :make-lists-3))
  :gettable-instance-variables)

(defmethod (flavor-4  :make-lists) ()	
  (list  flavor-4-iv ))

(defmethod (flavor-4  :make-lists-2) ()	
  (list  flavor-4-iv ))

(defmethod (flavor-4  :make-lists-3) ()	
  (list  flavor-4-iv ))

(defun multiple-append-test ()
  (setq *test-instance* (make-instance 'flavor-4))
  (test (equal '("flavor-3-iv" "flavor-1-iv" "flavor-2-iv" "flavor-4-iv")
               (send *test-instance* :make-lists))
	":append :base-flavor-first test")
  (test (equal '("flavor-4-iv" "flavor-2-iv" "flavor-1-iv" "flavor-3-iv")
	       (send *test-instance* :make-lists-2))
	":append :base-flavor-last test")
  (etest (send *test-instance* :make-lists-3)
	"method returns a non list, :base-flavor-first"))

(compile-flavor-methods flavor-1 flavor-3  flavor-2 flavor-4)

(multiple-append-test)
