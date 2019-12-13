;;; -*- Mode: LISP; Syntax: Common-lisp; Package: FLAVORS -*-
;;;
;;; comp-meth.lisp, Module FLAVORS
;;;
;;; *********************************************************************
;;;
;;;  (c) Copyright 1986, 1987, 1988 by Lucid Inc., All Rights Reserved
;;;
;;; *********************************************************************
;;;
;;; Edit-History:file to test compile-flavor-methods.  Used with file
;;; comp-load.lisp .
;;;
;;; Created: 1/29/86 by moe
;;;
;;; Reviewed: <Review-Date>
;;;
;;; 22-Sep-87 lnz: Include (setf (symbol-plist ...) ...) within eval-when.
;;;  3-Nov-88 hardy: added filename & Module to header, updated
;;;                  copyright notice
;;;
;;; End-of-Edit-History

(use-package 'flavors)

(defvar *test-list*)

(eval-when (eval compile load)

(setf (symbol-plist 'bar-mixin) '())	;make sure that flavor was
						;never defined
(setf (symbol-plist 'bar) '())

)

(defflavor bar-mixin () ())
(defmethod (bar-mixin :make-list) (n)
  (setq *test-list* (cons n (cons "bar-mixin-make-list" *test-list*))))
(defmethod (bar-mixin :before :make-list) (n)
  (setq *test-list* (cons n (cons "bar-mixin-before-make-list" *test-list*))))
(defmethod (bar-mixin :after :make-list ) (n)
  (setq *test-list* (cons n (cons "bar-mixin-after-make-list" *test-list*))))
(defflavor bar () (bar-mixin))
(defmethod (bar :make-list) (n)
  (setq *test-list* (cons n (cons "bar-make-list" *test-list*))))
(defmethod (bar :before :make-list) (n)
  (setq *test-list* (cons n (cons "bar-before-make-list" *test-list*))))
(defmethod (bar :after :make-list ) (n)
  (setq *test-list* (cons n (cons "bar-after-make-list" *test-list*))))

(compile-flavor-methods bar-mixin bar)
