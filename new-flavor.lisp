; Tasteful Flavors			-*- cold-load:t; Mode: common-Lisp; Package: SI; Base:8-*-

;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.
;;;
;;;                     TEXAS INSTRUMENTS INCORPORATED.
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151
;;;
;;; Copyright (C) 1985-1989 Texas Instruments Incorporated. All rights reserved.

;;Change history:
;; 07/01/88 clm for BJ - Allow integers for subtypes in FLAVOR-METHOD-ENTRY.
;; 12/13/88 clm -  Fixed PERFORM-FLAVOR-REDEFINITION so that the flavor's flavor-depended-on-by
;;                 information would not be lost. (spr 8982)
;;  3/16/89 DNG - Included changes to COMPILATION-FLAVOR and
;;		COMPILATION-DEFINE-FLAVOR for CLOS.  Removed code for #-Elroy.
;;  3/18/89 DNG - Integrated patches from "CLOS;FLAVOR-METACLASS".
;;  4/10/89 JLM - modified delete mle to setf the magic-list-entry.  This is safer.
;;  4/11/89 JLM - changed usage of (PUTPROP ... to (SETF (GET ...

; A flavor-name is a symbol which names a type of objects defined
; by the combination of several flavors.  The SI:FLAVOR
; property is a data-structure (of type FLAVOR) defining the
; nature of the flavor, as defined below.

; Flavors come in essentially three kinds.  The first kind defines a class
; of flavors, and provides the basic instance variables and methods for
; that class.  This kind typically includes only VANILLA-FLAVOR as a
; component, and uses the :REQUIRED-INSTANCE-VARIABLES and
; :REQUIRED-METHODS options.  The second kind of flavor represents a
; particular option that may be combined in (a "mix-in").  The third
; kind of flavor is the kind that can usefully be instantiated; it is
; a combination of one of the first kind and several of the second kind,
; to achieve the behavior desired for a particular application.

; The following symbols are interesting to outsiders:
; DEFFLAVOR - macro for defining a flavor
; DEFMETHOD - macro for defining a method
; DEFWRAPPER - macro for defining a flavor-wrapper
; INSTANTIATE-FLAVOR - create an object of a specified flavor
; MAKE-INSTANCE - easier to call version of INSTANTIATE-FLAVOR
; COMPILE-FLAVOR-METHODS - macro which does the right thing in the compiler
; RECOMPILE-FLAVOR - function to recompile a flavor and maybe any flavors
;		that depend on it.  Usually this happens automatically.
; DECLARE-FLAVOR-INSTANCE-VARIABLES - macro to put around a function
;		that will be called by methods and wants to access instance
;		variables.
; FUNCALL-SELF - a macro which, assuming you are a flavor instance, will
;		call yourself without bothering about rebinding the
;		variables.  Will do something totally random if SELF
;		isn't a flavor instance.
; LEXPR-FUNCALL-SELF - LEXPR-FUNCALL version of above
; *ALL-FLAVOR-NAMES* - list of all symbols which have been used as the name of a flavor
; *ALL-FLAVOR-NAMES-AARRAY* - completion aarray of flavor names to flavors.
;		Each flavor is included twice, once with and once without its package prefix.
; *FLAVOR-COMPILATIONS* - list of all methods which had to be compiled
;		this is useful for finding flavors which weren't compiled in qfasl files
;		or which need to be recompiled to bring them up to date.
; *FLAVOR-COMPILE-TRACE* - if non-NIL, a FORMAT destination for messages about
;		recompilation of combined methods
; *USE-OLD-FLAVOR-INFO* - if NIL, re-DEFFLAVORing a flavor always makes a new one.
;		For debugging weird screws.
;		Also makes it possible to redefine a flavor and leave old
;		instances with the old methods, even if the flavor instance variables
;		are not being changed.
; FLAVOR-ALLOWS-INIT-KEYWORD-P - determine whether a certain flavor allows
;		a certain keyword in its init-plist.
; FLAVOR-ALLOWED-INIT-KEYWORDS - returns all the init keywords a flavor handles.

; Roads not taken:
;  o Changing the size of all extant instances of a flavor.
;  o Nothing to stop you from instantiating a flavor of the first or
;    second kind.  In practice you will usually get an error if you try it.

; Philosophy with respect to multiple processes
;  Interrupts are inhibited such that multiple processes munging unrelated
;  flavors should work.  Multiple processes instantiating related flavors
;  will work, however multiple processes defining methods for the same
;  flavor at the same time, and things like that, will not.


(defun assq-careful (x y)
  (assoc x y :test #'eq))

(defun assq (x y)
  (assoc x y :test #'eq))

(setf (documentation 'self 'variable)
      "When a message is sent to a flavor instance, this special variable is
automatically bound to that object.")

(defvar *integrate-combined-methods* ()
   "When compiling a combined method, should the component methods be expanded inline?")
;;; Phd 10/4/85 add this new flag to allow more that 120 settable instance variables.


(defvar *flavor-enable-case-set-methods* t
   "Enable generation of :case :set methods on settable instance variables")

(defun string-append (&rest strings)
  (apply #'concatenate (append (list 'string) strings)))

(declaim (ftype make-run-time-alternative-combinations-1)
	 (ftype make-run-time-alternative-combinations-1)
	 )

;; In common lisp on modern architecture, no need to explicitly return
;; storage.
(defun return-storage (object &optional (force-p nil) &aux region object-origin object-size)
  "Dispose of OBJECT, returning its storage to free if possible.
If OBJECT is a displaced array, the displaced-array header is what is freed.
You had better get rid of all pointers to OBJECT before calling this,
 e.g. (RETURN-STORAGE (PROG1 FOO (SETQ FOO NIL)))
Returns T if storage really reclaimed, NIL if not."
  (declare (ignore region object-origin object-size force-p))
  (setq object nil)
  t)

(defun intern1 (pname &optional (pkg *package*))
  (prog1
    (intern pname pkg)
    (return-storage (prog1
		      pname
		      (setq pname ())))))

(defun make-run-time-alternative-defflavors (flavor-name specs)
  "return a list of defflavor forms for the run-time alternatives of flavor-name.
these are the flavors generated automatically by defining flavor-name
and one of which you get when you instantiate flavor-name.
specs should be the value of the :run-time-alternatives option in its definition;
this function can be called before the definition is really in effect."
  (loop for alt in (make-run-time-alternative-combinations-1 flavor-name specs) when
     (and (not (member-if 'stringp alt)) (> (length alt) 1)) collect
     `(defflavor ,(intern (combination-flavor-name alt)) () ,alt)))

(defun make-run-time-alternative-alist (flavor-name specs)
  (mapcar
   #'(lambda (combination)
       (cons combination (intern (combination-flavor-name combination))))
   (make-run-time-alternative-combinations-1 flavor-name specs)))

(defun combination-flavor-name (flavor-list &aux combined-name)
  (dolist (name (remove-duplicates flavor-list))
    (if (string-equal name "-FLAVOR" :start1 (- (length name) 7))
      (setq name (SUBSEQ NAME 0 (- (length name) 7))))
    (if (string-equal name "-MIXIN" :start1 (- (length name) 6))
      (setq name (SUBSEQ name 0 (- (length name) 6))))
    (if combined-name
      (setq combined-name (string-append combined-name "-" name))
      (setq combined-name name)))
  combined-name)




(defun make-run-time-alternative-combinations (flavor)
  "Return a list of flavor combinations which are run-time alternatives of FLAVOR-NAME.
Each combination is a list of the flavor names to be combined."
  (let ((specs (flavor-get flavor :run-time-alternatives)))
    (make-run-time-alternative-combinations-1 flavor specs)))

(defun make-run-time-alternative-combinations-1 (flavor-name specs)
  (if (null specs)
    (if flavor-name
      `((,flavor-name))
      '(nil))
    (let ((remaining-specs-alternatives
	   (make-run-time-alternative-combinations-1 flavor-name (cdr specs)))
	  (this-spec-alternatives (make-run-time-alternatives (car specs))))
      (loop for this-spec in this-spec-alternatives nconc
	 (loop for remaining in remaining-specs-alternatives collect
	    (append this-spec remaining))))))





;; This macro is used to define a flavor.  Use DEFMETHOD to define
;; methods (responses to messages sent to an instance of a flavor.
(defmacro defflavor (name instance-variables component-flavors &rest options)
 "INSTANCE-VARIABLES can be symbols, or lists of symbol and initialization.
 COMPONENT-FLAVORS are searched from left to right for methods,
  and contribute their instance variables.
 OPTIONS are:
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
  ;There may be more.
  (let ((copied-options (copy-list options)))
    `(progn
       (eval-when (load eval)
	  (defflavor2 ',name ',instance-variables ',component-flavors ',copied-options))
       (eval-when (compile)
	  (if (just-compiling)
	    (let ((*just-compiling* t))
	      (defflavor2 ',name ',instance-variables ',component-flavors ',copied-options)
	      (compose-automatic-methods (compilation-flavor ',name)))
	    (compose-automatic-methods (get ',name 'flavor))))
       (eval-when (eval) (compose-automatic-methods (get ',name 'flavor)))
       (eval-when (load eval)
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
