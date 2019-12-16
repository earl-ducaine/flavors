;; mit license

(in-package :flavors)

(defun pre-process-options (instance-variables options)
  (do ((opts options (cdr opts)))
      ((null opts)
       nil)
    (and (consp (car opts))
	 (eq (caar opts) :outside-accessible-instance-variables)
	 (return (cdar opts)))
    (and (eq (car opts) :outside-accessible-instance-variables)
	 (return
	   (mapcar #'(lambda (x)
		       (if (atom x)
			   x
			   (car x)))
		   instance-variables)))))

(defun run-pre-process-options ()
  (pre-process-options
   '()
   '((:DOCUMENTATION :BASE-FLAVOR
      "All streams are built on this. This flavor is mostly for typep,
       but also provides default methods for messages which all
       streams, input or output, are required to handle."))))

(defun process-options (instance-variables options)
  (do ((vs (pre-process-options instance-variables options) (cdr vs))
       (prefix
	(or (cadr (assq-careful :accessor-prefix options))
	    (string-append name "-")))
       (ords
	(do ((opts options (cdr opts)))
	    ((null opts)
	     nil)
	  (and (consp (car opts)) (eq (caar opts) :ordered-instance-variables)
	       (return (cdar opts)))
	  (and (eq (car opts) :ordered-instance-variables)
	       (return
		 (mapcar #'(lambda (x)
			     (if (atom x)
				 x
				 (car x)))
			 instance-variables)))))
       (res nil
	    (cons
	     `(defsubst ,(intern1 (string-append prefix (car vs))) (,name)
		(declare (function-parent ,name))
		,(if (member (car vs) ords :test #'eq)
		     `(%instance-ref ,name
				     ,(1+ (position (car vs) (the list ords) :test #'eq)))
		     `(symeval-in-instance ,name ',(car vs))))
	     res)))
      ((null vs)
       res)))


;; This macro is used to define a flavor.  Use DEFMETHOD to define
;; methods (responses to messages sent to an instance of a flavor.
(defmacro defflavor (name instance-variables component-flavors &rest options)
  "INSTANCE-VARIABLES can be symbols, or lists of symbol and initialization.
   COMPONENT-FLAVORS are searched from left to right for methods, and
   contribute their instance variables.  OPTIONS are:
  (:GETTABLE-INSTANCE-VARIABLES v1 v2...)
  (:SETTABLE-INSTANCE-VARIABLES v1 v2...)
  (:REQUIRED-INSTANCE-VARIABLES v1 v2...)
  (:REQUIRED-METHODS m1 m2...)
  (:REQUIRED-FLAVORS f1 f2...)
  (:INITTABLE-INSTANCE-VARIABLES v1 v2...)
  (:INIT-KEYWORDS k1 k2...)
  (:DEFAULT-INIT-PLIST k1 v1 k2 v2...)
  (:DEFAULT-HANDLER function)
  (:INCLUDED-FLAVORS f1 f2...)
  :NO-VANILLA-FLAVOR
  (:ORDERED-INSTANCE-VARIABLES v1 v2...)
  (:OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES v1 v2...)
  (:ACCESSOR-PREFIX sym)
  (:METHOD-ORDER m1 m2...)
  (:METHOD-COMBINATION (type order operation1 operation2...)...)
  (:DOCUMENTATION <args>...)
  (:SPECIAL-INSTANCE-VARIABLES <variables>)
  :ABSTRACT-FLAVOR
  :ALIAS-FLAVOR"
  ;; There may be more.
  (let ((copied-options (copy-list options)))
    `(progn
       (eval-when (:load-toplevel :execute)
	 (defflavor2 ',name ',instance-variables ',component-flavors ',copied-options))
       (eval-when (:compile-toplevel)
	 (if (just-compiling)
	     (let ((*just-compiling* t))
	       (defflavor2 ',name ',instance-variables ',component-flavors ',copied-options)
	       (compose-automatic-methods (compilation-flavor ',name)))
	     (compose-automatic-methods (get ',name 'flavor))))
       (eval-when (:execute)
	 (compose-automatic-methods (get ',name 'flavor)))
       (eval-when (:load-toplevel :execute)
	 ,@(do ((vs
		 (do ((opts options (cdr opts)))
		     ((null opts)
		      nil)
		   (and (consp (car opts))
			(eq (caar opts) :outside-accessible-instance-variables)
			(return (cdar opts)))
		   (and (eq (car opts) :outside-accessible-instance-variables)
			(return
			  (mapcar #'(lambda (x)
				      (if (atom x)
					  x
					  (car x)))
				  instance-variables))))
		 (cdr vs))
		(prefix
		 (or (cadr (assq-careful :accessor-prefix options)) (string-append name "-")))
		(ords
		 (do ((opts options (cdr opts)))
		     ((null opts)
		      nil)
		   (and (consp (car opts)) (eq (caar opts) :ordered-instance-variables)
			(return (cdar opts)))
		   (and (eq (car opts) :ordered-instance-variables)
			(return
			  (mapcar #'(lambda (x)
				      (if (atom x)
					  x
					  (car x)))
				  instance-variables)))))
		(res nil
		     (cons
		      `(defsubst ,(intern1 (string-append prefix (car vs))) (,name)
			 (declare (function-parent ,name))
			 ,(if (member (car vs) ords :test #'eq)
			      `(%instance-ref ,name
					      ,(1+ (position (car vs) (the list ords) :test #'eq)))
			      `(symeval-in-instance ,name ',(car vs))))
		      res)))
	       ((null vs)
		res)))
       ,@(make-run-time-alternative-defflavors name
					       (or
						(cdr
						 (assq-careful :run-time-alternatives options))
						(cdr (assq-careful :mixture options))))
       ',name)))

(defmacro defflavor-parse (name instance-variables component-flavors
			   &rest options)
  (format t "~a~%" (list name instance-variables component-flavors options)))
