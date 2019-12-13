;;; -*- Mode: LISP; Syntax: Common-lisp; Package: FLAVORS -*-
;;;
;;; misc-fu.lisp, Module FLAVORS
;;;
;;; *********************************************************************
;;;
;;;  (c) Copyright 1986, 1987, 1988 by Lucid Inc.,  All Rights Reserved
;;;
;;; *********************************************************************
;;;
;;; Edit-History:test file for flavor-allowed-init-keywords,
;;; flavor-allows-init-keywords-p, get-handler-for, symeval-in-instance,
;;; set-in-instance 
;;;
;;; Created: by moe 11/21/85
;;; Reviewed: <Review-Date>
;;; 27-Jan-86 moe: removed (in-packge 'flavors) ,use (use-package 'flavors)
;;; 28-Jan-86 moe: don't try to import lucid:test nor  etest
;;;
;;; 10-Mar-87 maj: package qualified get-handler-for (bug #01738)
;;; 19-Mar-87 susan: updated for running in an application environment
;;; 19-May-87 jos: fixed bug 2041 -- get-handler-for is treeshaken out
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

)

(defvar *test-instance*)

(defflavor mixin (mixin-iv required-iv ) ()
  (:initable-instance-variables mixin-iv required-iv )
  (:init-keywords :type)
  (:required-init-keywords :type))

(defmethod (mixin :mixin-method) ()
  ())

(defflavor base-flavor (base-iv required-base-iv ) (mixin)
  (:initable-instance-variables required-base-iv)
  (:init-keywords :required-base-keyword)
  (:required-init-keywords :required-base-keyword))

(defmethod (base-flavor :base-flavor-method) ()
  ())

(defun keywords-test ()
  (setq *test-instance* (make-instance 'base-flavor :type "some-type"
				       :mixin-iv "mixin-iv"
				       :required-iv "required-iv"
				       :required-base-iv "required-base-iv"
				       :required-base-keyword
				       "required-base-keyword"))
(if (not *treeshaken-lisp*)
(progn
  (test (equal 'mixin-mixin-method-primary
	       (flavors::get-handler-for *test-instance* :mixin-method))
	"get-handler-for on mixin")
  (test (equal 'base-flavor-base-flavor-method-primary
	       (flavors::get-handler-for *test-instance* :base-flavor-method))
	"get-handler-for on base-flavor")
  (test (null (flavors::get-handler-for *test-instance* :non-existent-method))
	"get-handler-for on non-existent-method")
))
  (test (equal '(:mixin-iv :required-base-iv :required-base-keyword
			   :required-iv :type)
	       (flavor-allowed-init-keywords 'base-flavor))
	"flavor-allowed-init-keywords test")
  (etest (flavor-allowed-init-keywords 'non-existent-flavor)
	 "flavor-allowed-init-keywords etest")
  (test (equal '(:mixin-iv :required-iv :type)
	       (flavor-allowed-init-keywords 'mixin))	;boo boo
	"flavor-allowed-init-keywords test on mixin")
  (test (equal 'mixin (flavor-allows-init-keyword-p 'mixin :mixin-iv))
	"flavor-allows-init-keyword-p test on mixin-iv")	;boo boo
(test (equal 'base-flavor (flavor-allows-init-keyword-p 'base-flavor :mixin-iv))
	"flavor-allows-init-keyword-p test on mixin-iv")
  (test (equal 'base-flavor
	       (flavor-allows-init-keyword-p 'base-flavor :required-base-iv))
	"flavor-allows-init-keyword-p test on base-iv")
(test (equal 'base-flavor
	       (flavor-allows-init-keyword-p 'base-flavor :required-base-keyword))
	"flavor-allows-init-keyword-p test on base-iv")
  (test (null (flavor-allows-init-keyword-p 'base-flavor :non-existent-iv))
	"flavor-allows-init-keyword-p test on non-existent key"))
(defun eval-in-instance-test ()
  (test (equal "mixin-iv" (symeval-in-instance *test-instance* 'mixin-iv))
	"symeval-in-instance for mixin-iv")
  (test (equal "required-iv" (symeval-in-instance *test-instance* 'required-iv))
	"symeval-in-instance for required iv")
  (test (equal "required-base-iv"
	       (symeval-in-instance *test-instance* 'required-base-iv))
	"symeval-in-instance for iv")
  (test (equal "new-mixin-iv" (set-in-instance *test-instance*
					       'mixin-iv "new-mixin-iv"))
	"set-in-instance for mixin")
  (test (equal "new-mixin-iv" (symeval-in-instance *test-instance*
						   'mixin-iv))
	"symeval-in-instance for mixin")
  (test (equal "new-required-iv"(set-in-instance *test-instance*
						 'required-iv "new-required-iv"))
	"set-in-instance for base iv")
  (test (equal "new-required-iv" (symeval-in-instance *test-instance*
						      'required-iv))
	"symeval-in-instance for base iv")
  (test (equal "new-required-base-iv"(set-in-instance *test-instance*
						      'required-base-iv
						      "new-required-base-iv"))
	"set-in-instance for req base iv")
  (test (equal "new-required-base-iv"(symeval-in-instance *test-instance*
							  'required-base-iv))
	"symeval-in-instance for req base iv")
  (etest (set-in-instance *test-instance* 'non-existent-iv
			  "non-existent-iv")
	 "set-in-instance error for non-existent iv")
  (etest (symeval-in-instance *test-instance* 'non-existent-iv)
	 "set-in-instance for req base iv")
  (etest (symeval-in-instance *test-instance* 'base-iv) 
	 "symeval-in-instance for unbound base iv")
  (test (equal "unbound" (symeval-in-instance *test-instance*
					      'base-iv t
					      "unbound"))
	       "symeval-in-instance for unbound base iv")
  (test (null (symeval-in-instance *test-instance* 'fb t))
	"symeval-in-instance for unbound with error-p t")
  (test (null (symeval-in-instance *test-instance* 'fb t 'blah))
	"symeval-in-instance for error-p t with unbound passed"))
(defun flavor-utilities-test()
  (keywords-test)
  (eval-in-instance-test))


(compile-flavor-methods mixin base-flavor)

(flavor-utilities-test)
