;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: (FLAVOR-INTERNALS (LISP SYSTEM)) -*-
;;; ***************************************************************************
;;;
;;;        Copyright (C) 1985 by Lucid Inc.,  All Rights Reserved
;;;
;;; ***************************************************************************

(in-package "FLAVORS")


;;; Definitions for the Method Cache.

(eval-when (eval compile)

(defconstant log-2-method-cache-size 10)
(defconstant method-cache-size (ash 1 log-2-method-cache-size))
(defconstant log-2-method-cache-entry-size 2)
(defconstant method-cache-entry-size (ash 1 2))
(defconstant method-cache-entry-mask
  (ash (1- method-cache-size) log-2-method-cache-entry-size))

(defvar *method-cache-table*)

(defmacro cache-method-index (key1 key2)
  `(lucid::logand&
     (lucid::logxor& (lucid::%field ,key1 0 lucid::bits-per-fixnum)
		     (lucid::%field ,key2 0 lucid::bits-per-fixnum))
     method-cache-entry-mask))

(defmacro cached-method (key1 key2)
  (lucid::once-only (key1 key2)
    `(let ((index (cache-method-index ,key1 ,key2)))
       (and (boundp '*method-cache-table*)
	    (eq (svref *method-cache-table* (lucid::+& index 0)) ,key1)
	    (eq (svref *method-cache-table* (lucid::+& index 1)) ,key2)
	    (svref *method-cache-table* (lucid::+& index 2))))))
  
(defmacro cache-method (key1 key2 value)
  (lucid::once-only (key1 key2)
    `(let ((index (cache-method-index ,key1 ,key2)))
       (unless (boundp '*method-cache-table*)
	 (setq *method-cache-table*
	       (with-static-area
		   (lucid::new-simple-vector (* method-cache-size
						method-cache-entry-size)))))
       (setf (svref *method-cache-table* (lucid::+& index 0)) ,key1)
       (setf (svref *method-cache-table* (lucid::+& index 1)) ,key2)
       (setf (svref *method-cache-table* (lucid::+& index 2)) ,value))))


(defmacro flush-method-cache ()
  `(when (boundp '*method-cache-table*)
     (lucid::simple-vector-fill *method-cache-table* 0 0 nil)))

)


;;;
;;; Random stuff and Environments.
;;;


;;; Takes a list of forms and returns values of a list of doc-strings
;;; and declares, and a list of the remaining forms.

(eval-when (eval compile load)
(defun extract-doc-and-declares (forms)
    (do ((forms forms (cdr forms))
         (docs nil (cons (car forms) docs)))
        ((or (endp forms)
             (and (not (stringp (car forms)))
                  (not (and (listp (car forms))
                            (eq (caar forms) 'declare)))))
         (values (nreverse docs) forms))))
)

(eval-when (eval compile)

(defmacro self-and-descriptor (instance) ;moe 2/19/86
  `(values ,instance (%instance-ref ,instance 0)))

)

;;;
;;; Environments.
;;;

(eval-when (eval compile)

(defstruct (iv-env (:print-function private-structure-printer)
                   (:constructor make-iv-env (vector)))
  (vector nil :read-only t)
  (bindings* t))

)

(defun iv-env-bindings (env)
  (let ((bin (iv-env-bindings* env)))
    (cond ((listp bin) bin)
          (t (setf (iv-env-bindings* env)
                   (let ((vec (iv-env-vector env))
                         (res nil))
                     (dotimes (i (length vec))
                       (push `(,(svref vec i) (iv ,(svref vec i))) res))
                     res))))))

;;;
;;; Defstructs
;;;

(eval-when (eval compile)

(defstruct (instance-descriptor (:type vector)
                                (:constructor internal-make-id
                                              (type env default-entry)))
  (send-fn 'flavor-send)
  type
  (table (make-hash-table :size 30 :test #'eq))
  default-entry
  (instantiated-p nil)
  (env nil :read-only t))

(defstruct (entry (:type vector))
  function
  map
  cmap)

)

(lucid::defstruct-runtime-slot-function entry cmap entry)
(lucid::defstruct-runtime-slot-function entry function entry)
(lucid::defstruct-runtime-slot-function entry map entry)
(lucid::defstruct-runtime-slot-function instance-descriptor env instance-descriptor)


(defun make-instance-descriptor (type env default-handler)
  (internal-make-id type env (make-entry :function default-handler)))

(eval-when (eval compile)

(defstruct (method (:print-function private-structure-printer)
                   (:predicate methodp)
                   (:constructor make-method (fn-name calls ivs current-symbol)))
  fn-name
  calls          ; List in reverse order.
  ivs            ; Vector of variable names or NIL.
  current-symbol)

)

(defun method-called-methods (method)
  (method-calls (symbol-value method)))

(defun method-defined-p (method-fn-name)
  (methodp (symbol-value method-fn-name)))

;;;
;;; Instance Descriptors.
;;;

#|
;;; add use of &funcall - 4/3/86 moe
(defmacro funcall-entry (self message entry &rest args)
  `(lucid::&funcall (entry-function ,entry) ,self ,message ,entry ,@args))
|#

(defmacro apply-entry (self message entry &rest args)
  `(apply (entry-function ,entry) ,self ,message ,entry ,@args))


;;; Special version of apply-entry to avoid consing.

(eval-when (eval compile)

(defmacro xapply-entry (self message entry arg1 arg1p arg2 arg2p
			     arg3 arg3p arg4 arg4p remaining-args)
  `(let ((function (entry-function ,entry)))
     (cond (,arg4p (apply function ,self ,message ,entry
			  ,arg1 ,arg2 ,arg3 ,arg4 ,remaining-args))
	   (,arg3p (funcall function ,self ,message ,entry ,arg1 ,arg2 ,arg3))
	   (,arg2p (funcall function ,self ,message ,entry ,arg1 ,arg2))
	   (,arg1p (funcall function ,self ,message ,entry ,arg1))
	   (t (funcall function ,self ,message ,entry)))))

)

(eval-when (eval compile load)

(defmacro get-message ()
    "Used in the body of a default handler, returns the message
    that invoked this handler."
    '%message)
)

(defsetf get-message () (new)
;  (declare (ignore new))
  new
  (error "Cannot setf get-message." new))

(defmacro defun-default-handler (name args &body body)
  "Twiddles args so the function can be called as a default handler.
    Also makes (get-message) work in the body."
  (multiple-value-bind (docs forms) (extract-doc-and-declares body)
    `(defun ,name (self %message %entry ,@args)
       ,@docs
       (declare (ignore self %message %entry))
       ,@forms)))

(eval-when (eval compile)

(defmacro set-symbol-function-default-handler (name args &body body)
  "Like defun-default-handler but evaluates name"
  (multiple-value-bind (docs forms) (extract-doc-and-declares body)
    `(setf (symbol-function ,name)
	   #'(lambda (self %message %entry ,@args)
	       ,@docs
	       (declare (ignore self %message %entry))
	       ,@forms))))

)

(defun flavor-send (instance message
		    &optional (arg1 nil arg1p)
			      (arg2 nil arg2p)
			      (arg3 nil arg3p)
			      (arg4 nil arg4p)
		    &rest remaining-args)
  (declare (dynamic-extent remaining-args))
  (multiple-value-bind (self id) (self-and-descriptor instance)
    (let* ((flav (instance-descriptor-type id))
	   (entry (cached-method flav message)))
      ;;if found msg in cache, call it
      (if entry
	  (xapply-entry self message entry arg1 arg1p arg2 arg2p
			arg3 arg3p arg4 arg4p remaining-args)
	  ;;otherwise, look for entry in the table
	  (let* ((table (instance-descriptor-table id))
		 (entry (gethash  message table))
		 (um nil))
	    ;;if entry  is found,cache it
	    (cond (entry
		   (cache-method flav message entry))
		  ;;otherwise,look for unc-msg
		  ((setq um (gethash :unclaimed-message table))
		   (when arg4p
		     (push arg4 remaining-args))
		   (setq arg4 arg3
			 arg4p arg3p
			 arg3 arg2
			 arg3p arg2p
			 arg2 arg1
			 arg2p arg1p
			 arg1 message
			 arg1p t)
		   (setq entry um))
		  ;;otherwise,look for default-entry
		  (t (setq entry
			   (instance-descriptor-default-entry
			    id))
		     (cache-method flav message entry)
		     ))
	    ;;call whatever you wound up with
	    (xapply-entry self message entry arg1 arg1p arg2 arg2p
			  arg3 arg3p arg4 arg4p remaining-args))))))


(defun handle-message (message instance-descriptor method)
  "The method must be defined before it can be a handler.
  (via internal-define-method or define-set-method or define-get-method)."
  (let ((table (instance-descriptor-table instance-descriptor)))
    (unless (null table)
      (let ((entry (make-entry :function method)))
        (do-map-method instance-descriptor entry)
        (setf (gethash message (instance-descriptor-table instance-descriptor))
              entry))))
  method)

(defun unhandle-message (message instance-descriptor)
  "Makes the given message unhandled."
  (let ((table (instance-descriptor-table instance-descriptor)))
    (unless (null table)
      (remhash message table))))

(defun get-handler (message inst-or-desc)
  "Returns the method-function-name of the method that handles message
  for instance or instance-descriptor inst-or-desc."
  (let (table)
    (cond
     ((instancep inst-or-desc)
      (multiple-value-bind (self id) (self-and-descriptor inst-or-desc)
        (setq table (instance-descriptor-table id))
        (unless (hash-table-p table)
          (do-instance-resizing self)
          (multiple-value-setq (self id) (self-and-descriptor self))
          (setq table (instance-descriptor-table id))
          (unless (hash-table-p table)
            (error "Internal error: resizing #<Random Instance ~X> didn't work."
                   (lucid::%pointer inst-or-desc))))))
     (t (setq table (instance-descriptor-table inst-or-desc))))
    (let ((entry (gethash message table)))
      (if entry (method-fn-name (symbol-value (entry-function entry)))))))


;;;
;;; Other instance-descriptor stuff.
;;;

(eval-when (eval compile)

(defmacro do-handlers (((name function) instance-descriptor) &body body)
  "(((message method-fn-name) instance-descriptor) . body)
  Does the body for each handler, with message and method-fn-name bound to
  each successive handler binding."
  `(block nil
     (let ((table (instance-descriptor-table ,instance-descriptor)))
       (unless (null table)
         (maphash #'(lambda (,name entry)
		      (declare (ignore ,name entry))
                      (let ((,function
                             (method-fn-name
                              (symbol-value (entry-function entry)))))
                        ,@body))
                  table)))))

)

(defun instantiate-instance-descriptor (instance-descriptor)
  "Returns the new instance, all ivs set to unbound."
  (let* ((len (length (iv-env-vector
                       (instance-descriptor-env instance-descriptor))))
         (new (alloc-instance len instance-descriptor)))
    (setf (instance-descriptor-instantiated-p instance-descriptor) t)
    new))

(defun resize-instances (instance-descriptor new-descriptor function)
  "Basically just changes the instance to be of a new instance-descriptor.
  Those slots not present in the previous descriptor get set to unbound.
  The function, which probably doesn't get called immediately, should
  (when it IS called) try to set the unbound variables to some reasonable
  value."
  (setf (instance-descriptor-instantiated-p new-descriptor)
        (instance-descriptor-instantiated-p instance-descriptor)
        (instance-descriptor-table instance-descriptor)
        (cons new-descriptor function)))

(defun do-instance-resizing (instance)
  (multiple-value-bind (inner id) (self-and-descriptor instance)
    (let* ((new-id (car (instance-descriptor-table id)))
           (fn (cdr (instance-descriptor-table id)))
           (old-env (instance-descriptor-env id))
           (new-env (instance-descriptor-env new-id))
           (old-vec (iv-env-vector old-env))
           (new-vec (iv-env-vector new-env)))
      (let ((new (alloc-instance (length new-vec) new-id)))
        (dotimes (i (length new-vec))
          (let* ((iv (svref new-vec i))
                 (old-pos (position iv old-vec)))
            (if old-pos
                (setf (%instance-ref new (1+ i))
                      (%instance-ref inner (1+ old-pos))))))
        (setf (%instance-ref instance 0) new)
        (funcall fn new)
        new))))


(defun freeze-entry (id entry)
  (when (eq (symbol-function (entry-function entry)) #'non-method)
    (do-map-method id entry))
  (let* ((sym (entry-function entry))
         (new (make-symbol (symbol-name sym))))
    (setf (symbol-function new) (symbol-function sym)
          (symbol-value new) (symbol-value sym)
          (entry-function entry) new))
  (let ((cmap (entry-cmap entry)))
    (dotimes (i (length cmap))
      (setf (svref cmap i) (freeze-entry id (svref cmap i))))))

(defun freeze-instances (instance-descriptor)
  "Makes the instances of this instance-descriptor deaf to changes in 
  method definition.  Use unfreeze-instance to wake it up again."
  (maphash #'(lambda (mess entry)
               (declare (ignore mess))
	       mess
               (freeze-entry instance-descriptor entry))
           (instance-descriptor-table instance-descriptor)))

(defun unfreeze-instances (instance-descriptor)
  "Undoes freeze-instances."
  (declare (special instance-descriptor))
  (maphash #'(lambda (mess entry)
               (declare (ignore mess))
	       mess
               (do-map-method instance-descriptor entry))
           (instance-descriptor-table instance-descriptor)))

;;;
;;; Methods.
;;;


(defun-default-handler non-method (&rest args)
  (multiple-value-bind (inner-self id) (self-and-descriptor self)
    (declare (ignore inner-self))
    inner-self
    (let ((fn (entry-function %entry)))
      (cond ((not (symbolp fn))
             (error "Internal bogusness: ~S handler for ~S frozen unmapped."
                    (get-message) self))
            ((eq fn (method-current-symbol (symbol-value fn)))
             (error "Undefined method ~A." fn))
            (t (do-map-method id %entry)
               (apply-entry self (get-message) %entry args))))))

(eval-when (eval compile)

(defmacro map-ivs (ivs instance-ivs)
  `(let ((ivs ,ivs)
         (instance-ivs ,instance-ivs))
     (let ((res (if ivs (make-array (length ivs)))))
       (dotimes (i (length ivs))
         (let ((pos (position (svref ivs i) instance-ivs)))
           (setf (svref res i) (if pos (1+ pos)))))
       res)))

)

;;; When we first map in a method, we make the cmap a simple vector.
;;; The first time we remap, we make it a fill-pointered adjustable vector and
;;; thereafter adjust it to the appropriate size. @#@#

(defun do-map-method (id entry)
  (let* ((structure (symbol-value (entry-function entry)))
         (ivs (method-ivs structure))
         (called-methods (method-calls structure))
         (instance-ivs (iv-env-vector (instance-descriptor-env id))))
    (let ((cmap (if called-methods (make-array (length called-methods))))
          (map (map-ivs ivs instance-ivs))
          (new-sym (method-current-symbol (symbol-value (entry-function entry)))))
      (do ((i (1- (length called-methods)) (1- i))
           (m called-methods (cdr m)))
          ((null m))
        (let ((entry (make-entry :function (car m))))
          (do-map-method id entry)
          (setf (svref cmap i) entry)))
      (setf (entry-cmap entry) cmap
            (entry-map entry) map
            (entry-function entry) new-sym))))

(defun remap-method
       (method-fn-name
        &optional
        (new-function-object
         (symbol-function (method-current-symbol (symbol-value method-fn-name)))))
  (let* ((structure (symbol-value method-fn-name))
         (new-symbol (make-symbol (symbol-name method-fn-name)))
         (current (method-current-symbol structure)))
    (setf (symbol-value new-symbol) (symbol-value current)
          (symbol-function new-symbol) new-function-object
          (symbol-function current) #'non-method
          (method-current-symbol structure) new-symbol)))


(defun update-method (fn-name ivs called-methods)
  (if (boundp fn-name)
      (let ((structure (symbol-value fn-name)))
        (if (and (equalp ivs (method-ivs structure))
                 (equalp called-methods (method-calls structure)))
            ;; No remapping necessary.  Set the current to the new function.
            ;; If we're still on the original, we needn't do anything.
            (unless (eq fn-name (method-current-symbol structure))
              (setf (symbol-function (method-current-symbol structure))
                    (symbol-function fn-name)
                    (symbol-function fn-name) #'non-method))
            (progn (setf (method-ivs structure) ivs
                         (method-calls structure) called-methods)
                   (remap-method fn-name (symbol-function fn-name)))))
      (let ((structure (make-method fn-name called-methods ivs fn-name)))
        (setf (symbol-value fn-name) structure))))

;;; When a method-call or method-apply expands, it sees if it finds the 
;;; called method in the list of methods this method is known to call.
;;; If so, it just references the corresponding slot
;;; (the last element gets slot zero) of the other-mapping-table.
;;; If not, it pushes the new method onto the front of the list,
;;; updates cmap (currently by remapping everything - ugh)
;;; and references the new slot.

(defvar *calling-ivs* nil)
(defvar *calling-method* nil)
(defvar *called-methods* nil)

;;; Compiled: sml expands, install-method gets correct values, 
;;; %calling-method disappears.
;;; Interpreted: %calling-method is part of env; specials are nil at
;;; runtime / expansion time.
;;;

(defmacro internal-define-method (method-fn-name env args body)
  "Method-fn-name is a method-function-name (i.e. a symbol nobody else knows about).
  Env is an iv-environment. Args is the arglist.
  Body is a list of forms.

  Expands to a form that, when evaluated, defines a handler."
  `(compiler-let ((*calling-method* ',method-fn-name)
                  (*calling-ivs* ',(iv-env-vector env))
                  (*called-methods* nil))
     (lucid::symbol-macro-let ((%calling-method ',method-fn-name)
			       (%calling-ivs ',(iv-env-vector env))
			       ,@(iv-env-bindings env))
       (defun-default-handler ,method-fn-name ,args ,@body)
       (install-method ,method-fn-name))))


(defmacro iv (name)
  (if *calling-method*
      `(%instance-ref self (svref (entry-map %entry)
                                  ,(position name *calling-ivs*)))
      `(%instance-ref self (svref (entry-map %entry)
                                  (position ',name %calling-ivs)))))

(eval-when (eval compile)

(defmacro iv-bound-p (name)
  (if *calling-method*
      `(slot-unbound-p self (svref (entry-map %entry)
                                   ,(position name *calling-ivs*)))
      `(slot-unbound-p self (svref (entry-map %entry)
                                   (position ',name %calling-ivs)))))

)

(defmacro find-method (method)
  (if *calling-method*
      (compiler-find-method method)
      `(interpreter-find-method ',method %calling-method self %entry)))

(defun interpreter-find-method (method caller self %entry)
  (do ((list (method-calls (symbol-value caller)) (cdr list))
       (len 0 (1+ len)))
      ((null list)
       (remap-method caller)
       (push method (method-calls (symbol-value caller)))
       (do-map-method (instance-descriptor self) %entry)
       len)
    (if (eq (car list) method)
        (return (length (cdr list))))))

(defun compiler-find-method (method)
  (do ((list *called-methods* (cdr list))
       (len 0 (1+ len)))
      ((null list)
       (push method *called-methods*)
       len)
    (if (eq (car list) method)
        (return (length (cdr list))))))

(defmacro install-method (method)
  `(update-method ',method ',(or *calling-ivs* '#()) ',*called-methods*))
#|
(defmacro method-call (method . args)
  "Macro used inside internal-define-method, analogous to funcall.
  Call like (method-call method-fn-name arg1 arg2...)."
  `(let* ((slot (find-method ,method))
          (entry (svref (entry-cmap %entry) slot)))
     (funcall-entry self (get-message) entry ,@args)))
|#

(defmacro method-apply (method . args)
  "Macro used inside internal-define-method, analogous to apply.
  Call like (method-apply method-fn-name arg1 arg2)."
  `(let* ((slot (find-method ,method))
          (entry (svref (entry-cmap %entry) slot)))
     (apply-entry self (get-message) entry ,@args)))

(defun define-set-method (method-fn-name var)
  "Defines a method that sets the given variable name."
  (let ((vec (make-array 1 :initial-element var)))
    (set-symbol-function-default-handler method-fn-name (new)
      (setf (%instance-ref self (svref (entry-map %entry) 0)) new))
    (update-method method-fn-name vec nil)))

(defun define-get-method (method-fn-name var)
  "Defines a method that returns the given named variable."
  (let ((vec (make-array 1 :initial-element var)))
    (set-symbol-function-default-handler method-fn-name ()
      (%instance-ref self (svref (entry-map %entry) 0)))
    (update-method method-fn-name vec nil)))
