;;; -*- Mode: LISP; Syntax: Common-lisp; Package: FLAVORS -*-
;;;
;;; *************************************************************************
;;;
;;; (c) Copyright 1986 by Lucid Inc., All Rights Reserved
;;;
;;; **************************************************************************
;;;
;;; Test file for :progn method-combination
;;;
;;; Edit-History:
;;;
;;; Created: 11/8/85 by moe
;;; Reviewed: <Review-Date>
;;;
;;; 27-Jan-86 moe: removed (in-packge 'flavors) ,use (use-package 'flavors)
;;; 28-Jan-86 moe: don't try to import lucid:test nor  etest
;;; 19-Mar-87 susan: modified for application environment....
;;;                  can't run due to undefmethod usage
;;;  8-Apr-87 tomh: fixed broken-eval stuff
;;; 22-Sep-87 lnz: Include (setf (symbol-plist ...) ...) within eval-when.
;;; 18-Nov-87 lnz: Removed format call.
;;;
;;; End-of-Edit-History


(in-package 'flavors)

(eval-when (eval compile load)

(setf (symbol-plist 'simple-mixin) '())

(setf (symbol-plist 'base-flavor-first) '())

(setf (symbol-plist 'base-flavor-last) '())

(setf (symbol-plist 'flavor-1) '())

(setf (symbol-plist 'flavor-2) '())

(setf (symbol-plist 'flavor-3) '())

(setf (symbol-plist 'flavor-4) '())

)

;;;  A list consed by the method :make-list
(defvar *test-list* '())

;;;simple flavor with one mixin, base flavor last

  (defflavor simple-mixin ((mixin-iv "mixin-iv") ) ()
    )

  (defmethod (simple-mixin :make-lists) ()
    (setq *test-list* (cons mixin-iv *test-list*)))

  (defflavor base-flavor-last ((base-iv "base-iv") )
    (simple-mixin) 
    (:method-combination (:progn :base-flavor-last :make-lists))
    :gettable-instance-variables)

  (defmethod (base-flavor-last :make-lists) ()	
    (setq *test-list* (cons base-iv *test-list*)))

  (defun test-base-flavor-last ()
    (setq *test-list* ())
    (setq *test-instance* (make-instance 'base-flavor-last))
    (test (equal '("mixin-iv" "base-iv") (send *test-instance* :make-lists))
	  "test simple progn, base-flavor-last")
    (undefmethod (base-flavor-last :make-lists))
    (setq *test-list* ())
    (test (equal '("mixin-iv") (send *test-instance* :make-lists))
	  "remove a component method")
    )

;;;simple flavor with one mixin, base flavor first


  (defflavor base-flavor-first ((base-iv "base-iv") )
    (simple-mixin) 
    (:method-combination (:progn :base-flavor-first :make-lists))
    :gettable-instance-variables)

  (defmethod (base-flavor-first :make-lists) ()	
    (setq *test-list* (cons base-iv *test-list*)))

  (defun test-base-flavor-first ()
    (setq *test-list* ())
    (setq *test-instance* (make-instance 'base-flavor-first))
    (test (equal '("base-iv" "mixin-iv") (send *test-instance* :make-lists))
	  "test simple progn, base-flavor-first")
    (undefmethod (base-flavor-first :make-lists))
    (setq *test-list* ())
    (test (equal '("mixin-iv") (send *test-instance* :make-lists))
	  "remove a component method")
    (undefmethod (simple-mixin :make-lists)) ;remove all methods
    (setq *test-list* ())
    (etest  (send *test-instance* :make-lists)
	    "try to call undefmethoded method")
    )

;;; define two mixins to the base flavor, each with own instance variables (iv's)

  (defflavor flavor-1 ((flavor-1-iv "flavor-1-iv")) ()
    :gettable-instance-variables)

  (defmethod (flavor-1 :make-lists) ()
    (setq *test-list* (cons flavor-1-iv *test-list*  )))

  (defflavor flavor-3  ((flavor-3-iv "flavor-3-iv") )
    ())

  (defmethod (flavor-3  :make-lists) ()
    (setq *test-list* (cons  flavor-3-iv *test-list* ))
    )

  (defflavor flavor-2 ((flavor-2-iv "flavor-2-iv") ) (flavor-1 flavor-3)
    :gettable-instance-variables	   )

  (defmethod (flavor-2  :make-lists) ()
    (setq *test-list* (cons  flavor-2-iv *test-list* ))
    )

  (defflavor flavor-4 ((flavor-4-iv "flavor-4-iv") )
    (flavor-2) 
    (:method-combination (:progn :base-flavor-first :make-lists))
    :gettable-instance-variables)

  (defmethod (flavor-4  :make-lists) ()	
    (setq *test-list* (cons  flavor-4-iv *test-list* )))

  (defun test-multiple-mixins ()
    (setq *test-list* ())
    (setq *test-instance* (make-instance 'flavor-4))
    (test (equal '("flavor-4-iv" "flavor-2-iv" "flavor-1-iv" "flavor-3-iv")
		 (send *test-instance* :make-lists))
	  "test simple progn, base-flavor-first")
    (undefmethod (flavor-1 :make-lists))
    (setq *test-list* ())
    (test (equal '("flavor-4-iv" "flavor-2-iv" "flavor-3-iv")
		 (send *test-instance* :make-lists))
	  "remove a component method")
    (undefmethod (flavor-2 :make-lists))
    (setq *test-list* ())
    (test (equal '("flavor-4-iv" "flavor-3-iv") (send *test-instance* :make-lists))
	  "remove another component method")
    (undefmethod (flavor-3 :make-lists))
    (setq *test-list* ())
    (test (equal '("flavor-4-iv" ) (send *test-instance* :make-lists))
	  "remove another component method")
    (undefmethod (flavor-4 :make-lists))
    (setq *test-list* ())
    (etest (send *test-instance* :make-lists)
	   "try to call undefmethoded method")
    )

(compile-flavor-methods  simple-mixin  base-flavor-last
			   base-flavor-first  flavor-1
			   flavor-3 flavor-2 flavor-4)
(test-base-flavor-last)
(test-base-flavor-first)
(test-multiple-mixins)
