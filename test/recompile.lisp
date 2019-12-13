;;; -*- Mode: LISP; Syntax: Common-lisp; Package: FLAVORS -*-
;;;  
;;;; RECOMPILE, Module FLAVORS
;;;
;;; ************************************************************************
;;;
;;;    (c) Copyright 1986, 1987, 1988 by Lucid Inc., All Rights Reserved
;;;
;;; ************************************************************************
;;;
;;; test file for recompile-flavor
;;;
;;; Edit-History:
;;;
;;; Created: 2/18/86 by moe
;;; 19-Mar-87 susan: modified so that won't run in application environment
;;;                  since can't work with internal defwrappers
;;;  8-Apr-87 tomh: fixed broken eval stuff
;;; 23-Oct-87 jlm: added let-binding for ti
;;;                random-symbol => *random-symbol*
;;;                *list* => *random-symbol*
;;; 19-Nov-87 tomh: removed format statement 
;;; 27-Dec-88 hardy: Updated copyright notice, added name & module to
;;;                  header, cleaned up header.
;;;
;;; End-of-Edit-History

(in-package 'flavors)

  (defvar *random-symbol* ())

  (defvar *foo* )

  (defvar *bar* )

  (defflavor recompile-test ()())

  (defmethod (recompile-test :wacka) (*random-symbol*)
    (setq *random-symbol* (cons 1 *foo*)))

  (defwrapper (recompile-test :wacka) (ignore . body)
    `(progn (setq *foo* ())
	    . ,body))

  (let ((ti (make-instance 'recompile-test)))
    (defwrapper (recompile-test :wacka) (ignore . body)
      `(progn (setq *foo* '(2))
	      . ,body))
    (recompile-flavor 'recompile-test :wacka)
    (test (equal '(1 2) (send ti :wacka *random-symbol*))
	  "test for recomp flavor for specific method")
    (defwrapper (recompile-test :init) (ignore . body)
      `(progn (setq *bar* '(4))
	      . ,body))
    (recompile-flavor 'recompile-test)
    (send ti :init)
    (test (equal '(4) *bar* ) "test for recomp flavor for all methods")
    )
