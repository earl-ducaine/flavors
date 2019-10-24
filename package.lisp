;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LUCID; Base: 10 -*-
;;;
;;; FLAVORS-PACKAGE, Module FLAVORS
;;;
;;; ***************************************************************************
;;;
;;;        Copyright (C) 1987,1988,1989 by Lucid Inc.,  All Rights Reserved
;;;
;;; ***************************************************************************

;;; Flavors Package Definition.

(defpackage :lucid
  (:use :cl)
  (:export :common
	   :defstruct-simple-predicate
	   :string-append))

(defpackage :flavors
  (:use :cl)
  (:nicknames :flavor-internals)
  (:import-from :lucid :string-append)
  (:shadow :defmethod
	   :make-instance
	   :find-method
	   :undefmethod
	   :method
	   :make-method)
  (:export  :*all-flavor-names*
	    :cleanup-all-flavors
	    :compile-flavor-methods
	    :continue-whopper
	    :continue-whopper-all
	    :defflavor
	    :defmethod
	    :defwhopper
	    :defwrapper
	    :flavor-allowed-init-keywords
	    :flavor-allows-init-keyword-p
	    :instancep
	    :lexpr-continue-whopper
	    :make-instance
	    :recompile-flavor
	    :self
	    :send
	    :set-in-instance
	    :symeval-in-instance
	    :undefflavor
	    :undefmethod
	    :vanilla-flavor
	    :without-cleaning-flavors))

  
(push :flavors *features*)



