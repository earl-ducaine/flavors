;;; FLAVORS-PACKAGE, Module FLAVORS
;;;
;;;
;;;        Copyright (C) 1987,1988,1989 by Lucid Inc.,  All Rights Reserved
;;;

;;; Flavors Package Definition.

(defpackage :lucid
  (:use :cl)
  (:export :common
	   :defstruct-simple-predicate
	   :string-append
	   :symbol-macro-let
	   :compiler-let))

(defpackage :flavors
  (:use :cl :system)
  (:nicknames :flavor-internals)
  (:import-from :lucid :string-append :symbol-macro-let :compiler-let)
  (:shadow :defmethod
	   :make-instance
	   :find-method
	   :undefmethod
	   :method
	   :make-method
	   :slot-value)
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
