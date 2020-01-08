
(defpackage :mit-flavors
  (:use :cl)
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
