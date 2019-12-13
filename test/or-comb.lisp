;;; -*- Mode: LISP; Syntax: Common-lisp; Package: FLAVORS -*-
;;;
;;; or-comb.lisp, Module FLAVORS
;;;
;;; **********************************************************************
;;;
;;;   (c) Copyright 1986, 1987, 1988 by Lucid Inc.,  All Rights Reserved
;;;
;;; **********************************************************************
;;;
;;; Test file for :or method combination.
;;;
;;; Edit-History:
;;;
;;; Created: 11/11/85 by moe
;;; Reviewed: <Review-Date>
;;;
;;; 27-Jan-86 moe: removed (in-packge 'flavors) ,use (use-package 'flavors)
;;; 28-Jan-86 moe: don't try to import lucid:test nor  etest
;;; 19-Mar-87 susan: modified for running in application environment
;;; 22-Sep-87 lnz: Include (setf (symbol-plist ...) ...) within eval-when.
;;; 18-Nov-87 lnz: Removed format call.
;;;  3-Nov-88 hardy: added filename & Module to header, updated 
;;;                  copyright notice, cleaned up header
;;;
;;; End-of-Edit-History

(use-package 'flavors)

(eval-when (eval compile load)

(setf (symbol-plist 'mixin) '())

(setf (symbol-plist 'base-flavor) '())

(setf (symbol-plist 'flavor-1) '())

(setf (symbol-plist 'flavor-2) '())

(setf (symbol-plist 'flavor-3) '())

(setf (symbol-plist 'flavor-4) '())

)

(defvar *test-instance*)

(defvar *test-list* '())

;;; simple test with one mixin

(defflavor mixin () ()
	   )

(defmethod (mixin :test) ()
  'mixin)

(defmethod (mixin :test-2) ()
  'mixin)

(defmethod (mixin :test-3) ()
  ())

(defmethod (mixin :test-4) ()
  'mixin)

(defflavor base-flavor ()
  (mixin) 
  (:method-combination (:or :base-flavor-last :test)
                       (:or :base-flavor-first :test-2)
		       (:or :base-flavor-last :test-3)
		       (:or :base-flavor-first :test-4))
  :gettable-instance-variables)

(defmethod (base-flavor :test) ()	
  'base )

(defmethod (base-flavor :test-2) ()
  'base)

(defmethod (base-flavor :test-3) ()
  'base)

(defmethod (base-flavor :test-4) ()
  ())

(defun simple-or-test ()
  (setq *test-instance* (make-instance 'base-flavor))
  (test (equal 'base (send *test-instance* :test))
	":or with :base-flavor-last")
  (test (equal 'mixin (send *test-instance* :test-2))
	":or with :base-flavor-first")
  (test (equal 'base (send *test-instance* :test-3))
	":or with null result in mixin")
  (test (equal 'mixin (send *test-instance* :test-4))
	":or with null result in base"))

;;; test with multiple mixins

(defflavor flavor-1 ((flavor-1-iv "flavor-1-iv")) ()
  :gettable-instance-variables)

(defmethod (flavor-1 :make-lists) ()
  (setq *test-list* (cons flavor-1-iv *test-list*  )))

(defmethod (flavor-1 :make-lists-2) ()
  (setq *test-list* (cons flavor-1-iv *test-list*  )))

(defmethod (flavor-1 :make-lists-3) ()
  nil)

(defmethod (flavor-1 :make-lists-4) ()
  nil)

(defflavor flavor-3  ((flavor-3-iv "flavor-3-iv") )
	   ())

(defmethod (flavor-3  :make-lists) ()
  (setq *test-list* (cons  flavor-3-iv *test-list* ))
  )

(defmethod (flavor-3  :make-lists-2) ()
  (setq *test-list* (cons  flavor-3-iv *test-list* ))
  )

(defmethod (flavor-3  :make-lists-3) ()
  (setq *test-list* (cons  flavor-3-iv *test-list* ))
  )

(defmethod (flavor-3  :make-lists-4) ()
  nil
  )

(defflavor flavor-2 ((flavor-2-iv "flavor-2-iv") ) (flavor-1 flavor-3)
   :gettable-instance-variables	   )

(defmethod (flavor-2  :make-lists) ()
  (setq *test-list* (cons  flavor-2-iv *test-list* ))
  )

(defmethod (flavor-2  :make-lists-2) ()
  (setq *test-list* (cons  flavor-2-iv *test-list* ))
  )

(defmethod (flavor-2  :make-lists-3) ()
  (setq *test-list* (cons  flavor-2-iv *test-list* ))
  )

(defmethod (flavor-2  :make-lists-4) ()
  nil
  )

(defflavor flavor-4 ((flavor-4-iv "flavor-4-iv") )
  (flavor-2) 
  (:method-combination (:or :base-flavor-first :make-lists)
                       (:or :base-flavor-last :make-lists-2)
		       (:or :base-flavor-first :make-lists-3)
		       (:or :base-flavor-last :make-lists-4))
  :gettable-instance-variables)

(defmethod (flavor-4  :make-lists) ()	
  (setq *test-list* (cons  flavor-4-iv *test-list* )))

(defmethod (flavor-4  :make-lists-2) ()	
  (setq *test-list* (cons  flavor-4-iv *test-list* )))

(defmethod (flavor-4  :make-lists-3) ()	
  (setq *test-list* (cons  flavor-4-iv *test-list* )))

(defmethod (flavor-4  :make-lists-4) ()	
  nil)

(defun multiple-or-test ()
  (setq *test-instance* (make-instance 'flavor-4))
  (setq *test-list* ())
  (test (equal '("flavor-3-iv")
                (send *test-instance* :make-lists))
	":or :base-flavor-first test")	
  (setq *test-list* ())
  (test (equal '("flavor-4-iv")
	       (send *test-instance* :make-lists-2))
	":or :base-flavor-last test")	
  (setq *test-list* ())
  (test  (equal '("flavor-3-iv") (send *test-instance* :make-lists-3))
	"null result in flavor-1, :base-flavor-first")
  (setq *test-list* ())
  (test (null (send *test-instance* :make-lists-4))
	"null result in all,:base-flavor-last"))

(defun or-combination-test ()
  (simple-or-test)
  (multiple-or-test))

(compile-flavor-methods mixin base-flavor flavor-1 flavor-3
			flavor-2 flavor-4)

(or-combination-test)
