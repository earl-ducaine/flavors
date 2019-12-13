;;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LUCID; Base: 10 -*-
;;;;
;;;; flav-bugc-te, Module TEST
;;;
;;; ***************************************************************************
;;;
;;;        (c) Copyright 1987, 1988 by Lucid Inc.,  All Rights Reserved
;;;
;;; ***************************************************************************
;;;
;;; File contains test code for flavors bugs that has to be run compiled.
;;;
;;; Programmer: Meekie
;;;
;;; Edit-History:
;;;
;;; Created: July 31, 1987
;;; 31-Jul-87 meekie: copied test for bug 1578 from flav-eg-te.lisp
;;;  4-Aug-87 meekie: changed = to <= in test for bug #1578
;;; 17-Sep-87 lnz: Moved defflavor's/defmethod's to toplevel.
;;;  3-Nov-88 hardy: cleaned up header, updated copyright notice
;;; 24-Jan-89 maj: foo ==> foo-flav-bugc
;;;
;;; End-of-Edit-History



;;; bug #1578
;;;
(defflavor foo-flav-bugc () ())
(defmethod (foo-flav-bugc :foo1) (x1) t)
(defmethod (foo-flav-bugc :foo2) (x1 x2) t)
(defmethod (foo-flav-bugc :foo3) (x1 x2 x3) t)
(defmethod (foo-flav-bugc :foo4) (x1 x2 x3 x4) t)
(defmethod (foo-flav-bugc :foo5) (x1 x2 x3 x4 x5) t)
(defmethod (foo-flav-bugc :foo6) (x1 x2 x3 x4 x5 x6) t)

  (when *test-compiled*
    (test (progn
	    (let ((x (make-instance 'foo-flav-bugc)))
	      (gc)
	      ;; Make sure cache is hit.
	      (send x :foo1 1)
	      (let ((original-gc-size (lucid::gc-size)))
		(send x :foo1 1)
		(send x :foo2 1 2)
		(send x :foo3 1 2 3)
		(send x :foo4 1 2 3 4)
		(send x :foo5 1 2 3 4 5)
		(send x :foo6 1 2 3 4 5 6)
		(let ((bytes-consed (- (lucid::gc-size) original-gc-size)))
		  ;; 24 bytes is normal, eventually we might need to parameterize
		  ;; this.
	  	  (if (<= bytes-consed #.(* 3 8))
 		      t
		      (progn
			(format t "SEND consed ~D bytes"bytes-consed)
			nil))))))
	  "bug #1578: FLAVORS-SEND conses much too much"))

