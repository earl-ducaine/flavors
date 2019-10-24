;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: (FLAVORS (LISP FLAVOR-INTERNALS)) -*-
;;; ***************************************************************************
;;;
;;;        Copyright (C) 1985 by Lucid Inc.,  All Rights Reserved
;;;
;;; ***************************************************************************

(in-package :flavors)

(defun make-env (var-list ordered required 
                 settable gettable initable)
  (with-stacks (var-stack default-stack)
    (dolist (o ordered)
      (vector-push-extend o var-stack)
      (vector-push-extend 'unsupplied default-stack))
    (let ((numordered (length var-stack))
          temp)
      (dolist (var var-list)
        (cond ((listp var)
               (cond ((setq temp (position (car var) var-stack))
                      (setf (aref default-stack temp) (cadr var)))
                     (t (vector-push-extend (car var) var-stack)
                        (vector-push-extend (cadr var) default-stack))))
              (t (cond ((setq temp (position var var-stack))
                        (setf (aref default-stack temp) 'unsupplied))
                       (t (vector-push-extend var var-stack)
                          (vector-push-extend 'unsupplied default-stack))))))
      (dolist (req required)
        (cond ((find req var-stack))
              (t (vector-push-extend req var-stack)
                 (vector-push-extend 'required default-stack))))
      (let* ((ables (make-array (length default-stack) :initial-element 0)))
        (dolist (s settable) (setf (ables-settable (var-able s)) t
                                   (ables-gettable (var-able s)) t
                                   (ables-initable (var-able s)) t))
        (dolist (g gettable) (setf (ables-gettable (var-able g)) t))
        (dolist (i initable) (setf (ables-initable (var-able i)) t))
        (make-method-env :numordered numordered
                         :vector (copy-seq var-stack)
                         :defaults (copy-seq default-stack)
                         :ables ables)))))


;;;
;;; Flavor definition.
;;;

(defun-default-handler default-handler (&rest args)
  (let ((message (get-message)))
    (cond ((get-handler :unhandled-message (instance-descriptor self))
           (apply #'send self :unhandled-message message args))
          ((get-handler :print-self (instance-descriptor self))
           (error "Unhandled message ~S in instance ~S." message self))
          (t (error "Unhandled message ~S in #<Random Instance ~X>."
                    message (lucid::%pointer self))))))

;;; Exported specials.

(defvar *all-flavor-names* () "The list of all defined flavors.")
(defvar *undefined-flavor-names* ()
  "List of referred-to but not defflavored flavors.")
(defvar *dirty-flavors* (make-array 100 :fill-pointer 0 :adjustable t)
  "A vector (with fill pointer) of instantiated flavors that need work.")

(defvar *flavor-compile-methods* t
  "If T, new combined methods will automatically be compiled.")

;;; When loading flavors, if lisp was built without the compiler set
;;;  *flavor-compile-methods* to nil (because you can't compile them...).
(unless (memq :compiler *features*)
  (setq *flavor-compile-methods* nil))

(defvar *default-combination*'(:daemon . :base-flavor-last)
  "Something like (:daemon . :base-flavor-last).")

;;;
;;; Internal specials.
;;;

;;; Options that alone mean all of the instance variables.
(defparameter *var-options*
  '(:gettable-instance-variables
    :settable-instance-variables :initable-instance-variables
    :ordered-instance-variables))

(eval-when (:execute :compile-toplevel)

  (defbits changed
      ;; Recompute all-components, compute everything if change.
  components            
  required-flavors      ; Check if we're instantiable.
  required-ivs          ; Ditto.
  iv-order              ; Maybe important to the kernel.
  iv-inits              ; Someday the inits will be in a function.
  all-methods           ; We only stay up-to-date if instantiated.  Redo all.
  required-methods      ; See if instantiable.
  required-inits        ; Recompute cached quantities after we know it's inst.
  default-plist         ; 
  init-keywords
  iv-keywords)

(defconstant %all-components-changed% #b11111111110)

(defbits flags
  vanilla-p
  abstract-p
  defined-p             ; Defflavored.
  compiled-p            ; Compiled and non-abstract means instantiable.
                        ; Compiled and abstract means compiled methods.
  wayward-p)            ; Has instances but is currently uninstantiable.

;;;More on method structures below
(defstruct (method-structure (:print-function private-structure-printer)
                             (:constructor make-method-structure
                                           (&optional methods)))
  methods
  (types (make-list (length methods))))

(defstruct (flavor (:print-function print-flavor))
  name
  (components nil)
  (included-flavors nil)
  (required-flavors nil)
  (required-methods nil)
  (default-plist nil)
  (init-keywords nil)
  (required-inits nil)
  (method-env nil)
  (combinations nil)    ; Assoc of name to combination 
  (prefix nil)

  (descriptor nil)
  (required-inits* nil)
  (init-keywords* nil)
  (default-plist* nil)
  (iv-keywords* nil)    ; Assoc of var to position?

  (dependents nil)
  (changed 1)
  (flags 0)
  (methods (make-method-structure))
  (all-components+ nil)
  (instance-env nil)
  (changed-methods nil)) ; Vector of lists.

(defun print-flavor (object stream depth)
  (declare (ignore depth))
  (format stream "#<Flavor ~S>" (flavor-name object)))

(defmacro flavor-defined-p (flavor)
    `(flags-defined-p (flavor-flags ,flavor)))

(defmacro flavor-has-vanilla-p (flavor)
    `(flags-vanilla-p (flavor-flags ,flavor)))

(defmacro flavor-abstract-p (flavor)
   `(flags-abstract-p (flavor-flags ,flavor)))

(defun flavor-compiled-p (flavor)
  (and (flavor-defined-p flavor)
       (flags-compiled-p (flavor-flags flavor))))

(defsetf flavor-compiled-p (flavor) (new)
  `(setf (flags-compiled-p (flavor-flags ,flavor)) ,new))

(defmacro flavor-wayward-p (flavor)
    `(flags-wayward-p (flavor-flags ,flavor)))

(defmacro flavor-instantiated-p (flavor)
    "Returns T for instantiated and wayward flavors."
    `(or (flavor-wayward-p ,flavor)
         (and (flavor-descriptor ,flavor)
              (instance-descriptor-instantiated-p (flavor-descriptor ,flavor)))))

)

(lucid::defstruct-runtime-slot-function flavor descriptor flavor)
(lucid::defstruct-runtime-slot-function flavor methods flavor)
(lucid::defstruct-runtime-slot-function flavor name flavor)


(defmacro get-flavor (name &optional createp)
  (let* ((const-createp (constantp createp))
	 (ev-createp (if const-createp (eval createp) nil)))
    `(let ((name ,name))
       (cond ((get name 'flavor))
	     ,@(when (or (not const-createp) ev-createp)
		 `((,createp (let ((res (make-flavor :name name)))
			       (setf (get name 'flavor) res)
			       (pushnew name *undefined-flavor-names*)
			       res))))
	     ,@(when (or (not const-createp) (not ev-createp))
		 `((t (error "Flavor ~S does not exist." name))))))))

(eval-when (:execute :compile-toplevel)

(defmacro flavor-dirty-p (flavor)
    `(or (plusp (flavor-changed ,flavor))
         (flavor-changed-methods ,flavor)))

(defmacro rework-flavor (flavor)
    `(let ((flavor ,flavor))
       (when (and (flavor-instantiated-p flavor)
                  (not (flavor-dirty-p flavor)))
         (vector-push-extend flavor *dirty-flavors*))))

(defmacro rework-methods (flavor methods)
  `(let ((flavor ,flavor))
     (cond ((null (flavor-changed-methods flavor))
	    (let ((stack (alloc-tiny-stack)))
	      (vector-push-extend ,methods stack)
	      (setf (flavor-changed-methods flavor) stack)))
	   (t (vector-push-extend ,methods (flavor-changed-methods flavor))))))

(defmacro do-inheriting-flavors ((var flavor &optional stack) &body body)
  `(cond ((eq (flavor-name flavor) 'vanilla-flavor)
	  (dolist (fl *all-flavor-names*)
	    (let ((,var (get-flavor fl)))
	      ,@(if stack `((vector-push-extend ,var ,stack)))
	      (when (flags-vanilla-p (flavor-flags ,var))
		,@body))))
	 (t ,(let* ((stack (or stack '%inheriting-flavors))
		    (body `((vector-push-extend ,flavor ,stack)
			    (do ((%i 0 (1+ %i)))
				((>= %i (length ,stack)))
			      (let ((,var (aref ,stack %i)))
				,@body)
			      (dolist (d (flavor-dependents (aref ,stack %i)))
				(unless (position d ,stack)
				  (vector-push-extend d ,stack)))))))
	       (if (eq stack '%inheriting-flavors)
		   `(with-stacks (,stack) ,@body)
		   `(progn ,@body))))))

  ;;; Check if name is a flavor.
  (defmacro flavorp (name)
    `(and (symbolp ,name)
	  (not (null (get ,name 'flavor)))))
)


;;; Returns the flavor symbol of which INSTANCE is an instance of.
(defun flavor-type-of (instance)
  (instance-descriptor-type 
    (instance-descriptor instance)))


;;; Returns T if instance is a member of FLAVOR directly or by
;;; inheretence.

(defun flavor-typep (instance flavor)
  (not (null 
	(member (get-flavor flavor) 
		(flavor-all-components  (get-flavor  (type-of
						      instance)))))))

;;; (FLAVOR-SUBTYPEP flavor-symbol1 flavor-symbol2)
;;; Determines if the flavor indicated by FLAVOR-SYMBOL1 is a
;;; subtype of the flavor indicated by flavor-symbol2 e.g.
;;; flavor1 inherets directly or indirectly from flavor2.

;;; This now just returns T or NIL.  The real subtypep turns that into (values
;;; t t) or (values nil t).

(defun flavor-subtypep (type1 type2)
  (and (flavorp type1)
       (flavorp type2)
       (member (get-flavor type2) 
	       (flavor-all-components (get-flavor type1)))))

(defparameter lucid::*flavors-flavor-symbol* 'flavor)
(defparameter lucid::*flavors-subtypep* #'flavor-subtypep)

;;;; TYPE-OF

;; (defadvice (type-of instance-type-of) (object)
;;       (flavor-type-of object)


;; (defadvice (type-of instance-type-of ) (object)
;;   (if (and (lucid::reasonablep object) (instancep object))
;;       (flavor-type-of object)
;;       (advice-continue  object)))

;;;; TYPEP
;;; The extra cond clause is for the case of foo having been
;;; defined as both a flavor and a structure.  So first check to
;;; see if it is the flavor - if not, check to see if it is the structure

;; (defadvice (typep flavor-type-p) (object type)
;;    (if (flavorp type)
;;        (cond
;; 	 ((instancep object)
;; 	  (flavor-typep object type))
;; 	 ((structurep object)
;; 	  (advice-continue object type)))
;;        (advice-continue object type)))

