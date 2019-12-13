;;; -*- Mode: LISP; Syntax: Common-lisp; Package: FLAVORS -*-
;;;
;;; comp-ld.lisp, Module FLAVORS
;;;
;;; *********************************************************************
;;;
;;;  (c) Copyright 1986, 1987, 1988 by Lucid Inc., All Rights reserved
;;;
;;; *********************************************************************
;;;
;;; Edit-History:
;;;
;;; Created: 1/29/86 After calling compile-flavor-methods, the
;;;                   flavor structure has an entry for flavors::description
;;;                   which in turn contains the message hash table.
;;; Reviewed: <Review-Date>
;;; 2/1/86 moe: change compile-file and load to use default test 
;;;             directory.
;;;
;;;  3-Oct-86 meg: inserted load/compile-test-file
;;; 22-nov-86 tomh: removed unixy pathnames
;;; 18-Nov-87 lnz: Removed format call.
;;;  3-Nov-88 hardy: added filename and Module to header, updated
;;;                  copyright notice.
;;;
;;; End-of-Edit-History

(use-package 'flavors)

(unless *broken-eval*

  (progn
    (defvar *test*)

    (setf (symbol-plist 'bar) ())		;make sure that flavor was
					;never defined
    (when *has-compiler*
      (compile-test-data *comp-meth*))
    (load-test-data *comp-meth*)

    (test (arrayp (flavors::flavor-descriptor (flavors::get-flavor 'bar)))
	  "after hash table generation") 

    ))















