;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LUCID; Base: 10 -*-
;;;
;;; FLAVORS-PACKAGE, Module FLAVORS
;;;
;;; ***************************************************************************
;;;
;;;        Copyright (C) 1987,1988,1989 by Lucid Inc.,  All Rights Reserved
;;;
;;; ***************************************************************************
;;;
;;; Flavors Package Definition.
;;;


(in-package "LUCID")


(eval-when (eval compile load)

(defpackage "FLAVORS"
  (:add-use-defaults t)
  (:use "SYSTEM")
  (:nicknames "FLAVOR-INTERNALS")
  (:shadow "DEFMETHOD" "MAKE-INSTANCE" "FIND-METHOD" "UNDEFMETHOD")
  (:export  "*ALL-FLAVOR-NAMES*"
	    "CLEANUP-ALL-FLAVORS"
	    "COMPILE-FLAVOR-METHODS"
	    "CONTINUE-WHOPPER"
	    "CONTINUE-WHOPPER-ALL"
	    "DEFFLAVOR"
	    "DEFMETHOD"
	    "DEFWHOPPER"
	    "DEFWRAPPER"
	    "FLAVOR-ALLOWED-INIT-KEYWORDS"
	    "FLAVOR-ALLOWS-INIT-KEYWORD-P"
	    "INSTANCEP"
	    "LEXPR-CONTINUE-WHOPPER"
	    "MAKE-INSTANCE"
	    "RECOMPILE-FLAVOR"
	    "SELF"
	    "SEND"
	    "SET-IN-INSTANCE"
	    "SYMEVAL-IN-INSTANCE"
	    "UNDEFFLAVOR"
	    "UNDEFMETHOD"
	    "VANILLA-FLAVOR"
	    "WITHOUT-CLEANING-FLAVORS")
  )

;;; end of eval-when(eval compile load)
)


;;;HP has this weird requirement to have strings that start with "@(#)"
;;;that describe their products.  Use "what programname" to find all the
;;;strings that describe all the components of the product.
;;;This is here for lack of a better place in the flavors submodule to put it.
#+(or HP300 PA)
(eval-when (compile)
  (defconstant *flavors-what-date* (current-time-string)))
#+(or HP300 PA)
(defconstant *flavors-what-string*
  #.(format nil "@(#) FLAVORS Revision: ~A Date: ~A "
	    #+HP300 "A.04.00" #+PA "B.04.00" *flavors-what-date*))


;;; (use-package "FLAVORS" "USER") ??
  
(push :flavors *features*)



