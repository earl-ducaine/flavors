;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: FLAVORS -*-
;;; ***************************************************************************
;;;
;;;        Copyright (C) 1985 by Lucid Inc.,  All Rights Reserved
;;;
;;; ***************************************************************************
;;;

(in-package :flavors)

(defmacro case-construct-methods (function-names function-methods)
  ;;; function-names=primary=(flavor-name-key-primary1
  ;;; flavor-name-key-primary2..) , function-methods= 
  ;;; ((METHOD-APPLY flavor-name-key-PRIMARY1 %COMBINED-ARGS)
  ;;;  (METHOD-APPLY flavor-name-key-PRIMARY2 %COMBINED-ARGS)...)
  `(let ((construct nil)
	(otherwise-clause nil)
	(fn-key nil))
    (do ((fn-name ,function-names (cdr fn-name))
	 (fn-meth ,function-methods (cdr fn-meth)))
	((null fn-name)  (progn
			   (when (null otherwise-clause)
			     (setq otherwise-clause
				   (list 'otherwise
					 `(error "unhandled case ~a"
					 keyform))))
			   (reverse (push otherwise-clause
					  construct))))
      (setq fn-key (get (car fn-name) :case))
      ;;; "otherwise" keys of 'otherwise and :otherwise are supported
      (if (eq :otherwise fn-key)
	  (setq otherwise-clause (list 'otherwise
				       (substitute '(push keyform
						     %combined-args)
						   '%combined-args
				       (car fn-meth))))
	  (push (list fn-key (car fn-meth))
		construct)))))


(defcombination-ordering daemon-ordering-primary (order)
  (order-wrappers :base-flavor-first 'wrappers '(:wrapper :whopper))
  (order-methods :primary 'primary '(:primary))
  (order-methods :primary 'default '(:default))
  (case order
    (:base-flavor-last
     (order-methods :base-flavor-last 'befores '(:before))
     (order-methods :base-flavor-first 'afters '(:after)))
    (:base-flavor-first
     (order-methods :base-flavor-last 'befores '(:before))	;changed my moe 11/12/85
     (order-methods :base-flavor-first 'afters '(:after)))
    (t (error "Unknown ordering ~S." order))))

(defcombination-ordering daemon-ordering-list (order)
  (order-wrappers :base-flavor-first 'wrappers '(:wrapper :whopper))
  (case order
    (:base-flavor-last
     (order-methods :base-flavor-last 'primary '(:primary))
     (order-methods :base-flavor-last 'default '(:default))
     (order-methods :base-flavor-last 'befores '(:before))
     (order-methods :base-flavor-first 'afters '(:after)))
    (:base-flavor-first
     (order-methods :base-flavor-first 'primary '(:primary))
     (order-methods :base-flavor-first 'default '(:default))
     (order-methods :base-flavor-first 'befores '(:before))
     (order-methods :base-flavor-last 'afters '(:after)))
    (t (error "Unknown ordering ~S." order))))

(defcombination :daemon daemon-ordering-primary (arg)
  (let ((primary (or (method-list 'primary) (method-list 'default))))
    (wrapper-mix 'wrappers
      (cond ((and (null (method-list 'befores)) (null (method-list 'afters)))
             (car (call-methods 'primary)))
            (t `(progn ,@(call-methods 'befores)
                       (multiple-value-prog1
                        ,@(or (call-methods primary) '(nil))
                        ,@(call-methods 'afters))))))))

(defcombination :progn daemon-ordering-list (arg)
  (let ((primary (or (method-list 'primary) (method-list 'default))))
    (wrapper-mix 'wrappers
                 (cond
		   ((null primary)		;added by moe 11/9/85
		    ())				;flags internal-cleanup-flavor
						;that there is not methods left
						;for this message
		  ;((and (null (method-list 'befores)) ;don't know why this was
		  ;	 (null (method-list 'afters))) ;here at all- moe 11/8/85
		  ; (car (call-methods primary)))
		   (t `(progn ,@(call-methods 'befores)
			      (multiple-value-prog1
				(progn ,@(call-methods primary))
				,@(call-methods 'afters))))))))

(defcombination :and daemon-ordering-list (arg) ;added by moe 11/11/85
  (let ((primary (or (method-list 'primary) (method-list 'default))))
    (wrapper-mix 'wrappers
                 (cond
		   ((null primary)		
		    ())	
		   (t `(progn ,@(call-methods 'befores)
			      (multiple-value-prog1
				(and ,@(call-methods primary))
				,@(call-methods 'afters))))))))

(defcombination :or daemon-ordering-list (arg) ;added by moe 11/11/85
  (let ((primary (or (method-list 'primary) (method-list 'default))))
    (wrapper-mix 'wrappers
                 (cond
		   ((null primary)		
		    ())	
		   (t `(progn ,@(call-methods 'befores)
			      (multiple-value-prog1
				(or ,@(call-methods primary))
				,@(call-methods 'afters))))))))

(defcombination :list daemon-ordering-list (arg) ;added by moe 11/11/85
  (let ((primary (or (method-list 'primary) (method-list 'default))))
    (wrapper-mix 'wrappers
                 (cond
		   ((null primary)		
		    ())	
		   (t `(progn ,@(call-methods 'befores)
			      (multiple-value-prog1
				(list ,@(call-methods primary))
				,@(call-methods 'afters))))))))

(defcombination :append daemon-ordering-list (arg) ;added by moe 11/11/85
  (let ((primary (or (method-list 'primary) (method-list 'default))))
    (wrapper-mix 'wrappers
                 (cond
		   ((null primary)		
		    ())	
		   (t `(progn ,@(call-methods 'befores)
			      (multiple-value-prog1
				(append ,@(call-methods primary))
				,@(call-methods 'afters))))))))

(defcombination :nconc daemon-ordering-list (arg) ;added by moe 11/12/85
  (let ((primary (or (method-list 'primary) (method-list 'default))))
    (wrapper-mix 'wrappers
                 (cond
		   ((null primary)		
		    ())	
		   (t `(progn ,@(call-methods 'befores)
			      (multiple-value-prog1
				(nconc ,@(call-methods primary))
				,@(call-methods 'afters))))))))

(defcombination :case daemon-ordering-list (arg) ;added by moe 9/28
  (let ((primary (or (method-list 'primary) (method-list 'default))))
    (wrapper-mix 'wrappers
                 (cond
		   ((null primary)		
		    (progn (format t "~% warning:no primary methods found")
			   nil))
		   (t `(progn ,@(call-methods 'befores)
			      (multiple-value-prog1
				  (let ((keyform (car %combined-args)))
				    (setq %combined-args (cdr %combined-args))
				    (case keyform
				      ,@(case-construct-methods
				    primary (call-methods primary))))
				,@(call-methods 'afters))))))))

(defflavor vanilla-flavor () ())

(defmethod (vanilla-flavor print) (stream depth)
  (send self :print-self stream depth))

(defmethod (vanilla-flavor describe) ()
  (send self :describe))

(defmethod (vanilla-flavor typep) (type)
  (let ((flavor (get type 'flavor)))
    (and flavor
         (not (null (member flavor
                            (flavor-all-components
                             (get-flavor
                              (instance-descriptor-type
                               (instance-descriptor self))))))))))

(defmethod (vanilla-flavor :print-self) (stream depth)
  (declare (ignore depth))
  depth
  (format stream "#<Instance ~A ~X>"
          (instance-descriptor-type (instance-descriptor  self))
          (lucid::%pointer self)))

(defmethod (vanilla-flavor :describe) ()
  (let* ((name (instance-descriptor-type (instance-descriptor self)))
         (vec (iv-env-vector (flavor-instance-env (get-flavor name)))))
    (format t "~&An instance of flavor ~S.~%" name)
    (cond ((zerop (length vec))
           (format t "~&No instance variables.~%"))
          (t (format t "~&Instance variables:~%")))
    (dotimes (i (length vec))
      (format t "~&~S ~A~%" (aref vec i)  ;change to ~A to print a string
	                                ;moe 12/17/85
              (symeval-in-instance self (aref vec i) t "unbound"))))
  self)

(defmethod (vanilla-flavor :which-operations) ()
  (let ((list nil))
    (do-handlers ((name function)
                  (flavor-descriptor
                   (get-flavor
                    (instance-descriptor-type
                     (instance-descriptor self)))))
      (declare (ignore function))
      function
      (push name list))
    list))

(defmethod (vanilla-flavor :operation-handled-p) (operation)
  (not (null (get-handler operation (instance-descriptor self)))))

(defmethod (vanilla-flavor :send-if-handles) (operation &rest arguments)
  (when (get-handler operation (instance-descriptor self))
    (apply #'send self operation arguments)))
