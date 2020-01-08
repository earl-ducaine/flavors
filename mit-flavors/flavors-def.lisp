

;; (c) Copywrite 1983, Massachusetts Institute of Technology
;;
;; This file contains some of the support macros that are need by the
;; flavor system.

(in-package :mit-flavors)


;; The data-structure on the FLAVOR property of a flavor-name
(defstruct (flavor :named)
  flavor-bindings		;List of locatives to instance variable
				; internal value cells.  MUST BE CDR-CODED!!
				;Fixnums can also appear.  They say to skip
				;whatever number of instance variable slots.
  flavor-method-hash-table	;The hash table for methods of this flavor.
				; NIL means method-combination not composed yet.
  flavor-name			;Symbol which is the name of the flavor.
				; This is returned by TYPEP.
  flavor-local-instance-variables	;Names and initializations,
					; does not include inherited ones.
  flavor-all-instance-variables	;Just names, only valid when "flavor
				; combination" composed.  Corresponds directly
				; to FLAVOR-BINDINGS and the instances.
  flavor-method-table		;Defined below.
  ;; End of locations depended on in many other files.
  flavor-depends-on		;List of names of flavors incorporated into this flavor.
  flavor-depended-on-by		;List of names of flavors which incorporate this one.
				;The above are only immediate dependencies.
  flavor-includes		;List of names of flavors to include at the end
				; rather than as immediate depends-on's.
  flavor-depends-on-all		;Names of all flavors depended on, to all levels, including
				; this flavor itself.  NIL means flavor-combination not
				; composed yet.  This is used by TYPEP of 2 arguments.
  (flavor-which-operations nil)	;List of operations handled, created when needed.
				; This is NIL if it has not been computed yet.
  ;; Redundant copy of :DEFAULT-HANDLER property, for speed in calling it.
  (flavor-default-handler nil)
  (flavor-gettable-instance-variables nil)
  (flavor-settable-instance-variables nil)
  (flavor-initable-instance-variables nil)
				;Alist from init keyword to name of variable
  (flavor-init-keywords nil)			;option
  (flavor-plist nil)		;Esoteric things stored here as properties
				;Known: :ORDERED-INSTANCE-VARIABLES, :DEFAULT-HANDLER
				; :OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES, :ACCESSOR-PREFIX,
				; :REQUIRED-INSTANCE-VARIABLES, :REQUIRED-METHODS,
				; :REQUIRED-FLAVORS, :SELECT-METHOD-ORDER,
				; :DEFAULT-INIT-PLIST, :DOCUMENTATION, :NO-VANILLA-FLAVOR
				; :GETTABLE-INSTANCE-VARIABLES :SETTABLE-INSTANCE-VARIABLES
				; ADDITIONAL-INSTANCE-VARIABLES
				; COMPILE-FLAVOR-METHODS
				; MAPPED-COMPONENT-FLAVORS
				; INSTANCE-VARIABLE-INITIALIZATIONS
				; ALL-INITABLE-INSTANCE-VARIABLES
				; REMAINING-DEFAULT-PLIST
				; REMAINING-INIT-KEYWORDS
				;The convention on these is supposed to be that
				;ones in the keyword packages are allowed to be
				;used by users.
				;Some of these are not used by the flavor system, they are
				;just remembered on the plist in case anyone cares.  The
				;flavor system does all its handling of them during the
				;expansion of the DEFFLAVOR macro.
  )

;; Todo -- ed -- This part of the flavor system will need to be
;; converted to modern CL closures once it's clear what the logic is,
;; namely the call to, fclosure-function and fclosurep.
(defun instancep (x)
  (and (fclosurep x) (eq (fclosure-function x) #'flavor-dispatch)))

(defvar self ()
  "Self referential pointer for flavors")

(defmacro send (object message &rest args)
  (if (eq object 'self)
      `(send-self ,message ,@args)
      `(send-internal ,object ,message ,@args)))

(defmacro lexpr-send (object &rest args)
  (if (eq object 'self)
      `(lexpr-send-self ,@args)
      `(lexpr-funcall #'send-internal ,object ,@args)))

;; These two functions are used when sending a message to yourself, for
;; extra efficiency.  They avoid the variable unbinding and binding
;; required when entering a closure.
(defmacro send-self (message &rest args)
  `(funcall (or (gethash ,message (flavor-method-hash-table .own-flavor.))
		(flavor-default-handler .own-flavor.))
	    ,message . ,args))

(defmacro funcall-self (&rest args) `(send-self . ,args))

(defmacro lexpr-send-self (message &rest args)
  `(lexpr-funcall (or (gethash ,message
			       (flavor-method-hash-table .own-flavor.))
		      (flavor-default-handler .own-flavor.))
		  ,message . ,args))

(defmacro lexpr-funcall-self (&rest args) `(lexpr-send-self . ,args))

(defun neq (value-1 value-2)
  (not (eq value-1 value-2)))

;; i.e. convent from keyword to symbol
(defun remove-colon (symbol)
  (if (symbolp symbol)
      (intern (symbol-name symbol))
      (error "remove-colon now only works on symbols, not names of symbols")))

(defun (setf send) (value expression)
  (if (or (atom (caddr expression))
	  (neq (car (caddr expression)) 'quote))
      (error "Don't know how to setf this ~S" expression))
  (cond ((eq (cadr (caddr expression)) ':get)
	 `(send ,(cadr expression) ':putprop ,value ,(cadddr expression)))
	(t
	 `(send ,(cadr expression)
		',(intern (format () ":set-~A"
				  (remove-colon (cadr (caddr expression)))))
		,value))))


(defmacro defprop (symbol value property)
  "make the value of symbol's property property be value."
  `(progn
     (setf (get ',symbol ',property) ',value)
     ',symbol))


;; (putprop 'flavorm t 'version)
