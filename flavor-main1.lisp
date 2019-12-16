;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: (FLAVORS (LISP FLAVOR-INTERNALS)) -*-
;;; ***************************************************************************
;;;
;;;        Copyright (C) 1985 by Lucid Inc.,  All Rights Reserved
;;;
;;; ***************************************************************************
;;;

(in-package "FLAVORS")

(defun message-clean-p (flavor method)
  (not (or (changed-all-methods (flavor-changed flavor))
           (let ((stack (flavor-changed-methods flavor)))
             (dotimes (i (length stack))
               (let ((elt (aref stack i)))
                 (when (or (eq method elt) (member method elt))
                   (return t))))))))

(defun reconstruct-defflavor (name)
  "Returns a call to defflavor that was something pretty close to how
  the flavor was defined."
  (let* ((flavor (get-flavor name)))
    (unless (flavor-defined-p flavor)
      (error "Flavor ~S has not been defflavored." name))
    (let* ((env  (flavor-method-env flavor))
           (vec (iv-env-vector env))
           (defaults (method-env-defaults env))
           (ables (method-env-ables (flavor-method-env flavor)))
           (vars nil) (initable nil) (settable nil) (gettable nil)
           (required nil) (ordered nil)
           (options nil))
      (let ((numordered (method-env-numordered env)))
        (dotimes (i (length vec))
          (let ((able (aref ables i))
                (var (aref vec i))
                (def (aref defaults i)))
            (case def
              (unsupplied (push var vars))
              (required (push var required))
              (t (push (list var def) vars)))
            (cond ((ables-settable able) (push var settable))
                  (t (when (ables-initable able) (push var initable))
                     (when (ables-gettable able) (push var gettable))))
            (when (< i numordered) (push var ordered)))))
      (if initable (push `(:initable-instance-variables ,@initable) options))
      (if settable (push `(:settable-instance-variables ,@settable) options))
      (if gettable (push `(:gettable-instance-variables ,@gettable) options))
      (if required (push `(:required-instance-variables ,@required) options))
      (if ordered (push `(:ordered-instance-variables ,@(nreverse ordered))
                        options))
      (if (not (flavor-has-vanilla-p flavor)) (push :no-vanilla-flavor options))
      (if (flavor-abstract-p flavor) (push :abstract-flavor options))
      (if (flavor-init-keywords flavor)
          (push `(:init-keywords ,@(flavor-init-keywords flavor)) options))
      (if (flavor-default-plist flavor)
          (push `(:default-init-plist ,@(flavor-default-plist flavor)) options))
      (if (flavor-required-inits flavor)
          (push `(:required-init-keywords ,@(flavor-required-inits flavor))
                options))
      (if (flavor-required-methods flavor)
          (push `(:required-methods ,@(flavor-required-methods flavor)) options))
      (if (flavor-required-flavors flavor)
          (push `(:required-flavors ,@(flavor-required-flavors flavor)) options))
      (if (flavor-included-flavors flavor)
          (push `(:included-flavors ,@(flavor-included-flavors flavor)) options))
      (if (not (equalp (flavor-prefix flavor)
                       (concatenate 'string (symbol-name (flavor-name flavor))
                                    "-")))
          (push `(:accessor-prefix ,(intern (flavor-prefix flavor)
                                            (symbol-package (flavor-name flavor))))
                options))
      (when (flavor-combinations flavor)
        (let ((combos nil))
          (dolist (cons (flavor-combinations flavor))
            (push (car cons) (my-assoc (cdr cons) combos)))
	  (push `(:method-combination
		  ,@(mapcar #'(lambda (list)
				`(,(caar list) ,(cdar list) ,@(cdr list)))
			    combos))
		options)))
      `(defflavor ,(flavor-name flavor) ,(nreverse vars)
         ,(flavor-components flavor)
         ,@options))))



;;;
;;; Method structures
;;;

;;; defstruct method-structure is before defstruct flavor, so that
;;; make-method-structure is defined in time for default-init.

(defun %get-method-types (name structure &optional create)
  "Returns a list whose car is the list of types."
  (let ((pos (position name (method-structure-methods structure))))
    (if pos (nthcdr pos (method-structure-types structure))
        (when create
          (push name (method-structure-methods structure))
          (push nil (method-structure-types structure))))))

(eval-when (:execute :compile-toplevel)

(defmacro get-method-types (name structure &optional create)
  `(%get-method-types ,name ,structure ,create))

(defmacro do-methods ((var structure) &body body)
  `(let ((%str ,structure))
     (macrolet ((get-method-types (name str &optional create)
		  (cond ((and (eq name ',var) (eq str ',structure)) '(car %list))
			(t `(%get-method-types ,name ,str ,create)))))
       (do ((%m (method-structure-methods %str) (cdr %m))
	    (%list (method-structure-types %str) (cdr %list)))
	   ((null %m))
	 (unless (null (car %list))
	   (let ((,var (car %m)))
	     ,@body))))))

(defmacro method-types (name structure)
  `(car (get-method-types ,name ,structure)))

(defsetf method-types (name structure) (new)
  `(progn				;changed to progn by moe 11/4/85
	(setf (car (get-method-types ,name ,structure t)) ,new)
	,new))

)

(defun method-add (name type fn-name structure)
  (let* ((case-method-p nil)
	 ;;; use (listp name) to determine wether you are
	 ;;; treating the case method type.  THis ony
	 ;;; works because case is the only type for which
	 ;;; name is pased as (:method-name :case-selector)
	 ;;; A kludge,but otherwise you have to pass in
	 ;;; the flavor-combinations structure
	 (list (get-method-types (if (listp name)
				     (progn (setq case-method-p t)
					    (car name))
				     name)
				     structure t)))
    (Let* ((assoc (assoc type (car list)))
	   (temp-name (cons :primary fn-name)))
      (cond (assoc
             (cond ((eq (cdr assoc) fn-name) nil)
		   (case-method-p
		    (setf (get fn-name :case) (cdr name))
		    (unless (member temp-name (car list) :test #'equal)
		      (push temp-name (car list))))
                   (t (setf (cdr assoc) fn-name) t)))
	    (case-method-p
	     (setf (get fn-name :case) (cdr name))
	     (push (cons :primary fn-name) (car list)))
	    (t (push (cons type fn-name) (car list))
               t)))))

(defun method-find (name type structure)
  (let ((list (get-method-types name structure)))
    (and list (cdr (assoc type (car list))))))

;;;
;;; Components, environment
;;;


(defun flavor-add-component (flavor components-stack undefined-flavors)
  (vector-push-extend flavor components-stack)
  (dolist (c (flavor-components flavor))
    (setq c (get-flavor c t))
    (cond ((flavor-defined-p c)
	   (unless (find c components-stack)
	     (setf undefined-flavors
		   (flavor-add-component
		    c components-stack undefined-flavors))))
	  (t (push (flavor-name c) undefined-flavors))))
  undefined-flavors)

(defun flavor-add-included (flavor
			    i
			    components-stack
			    second-stack
			    undefined-includeds)
  (cond ((not (flavor-defined-p flavor))
	 (push (flavor-name flavor) undefined-includeds))
	((or (find flavor components-stack :start i)
	     (find flavor second-stack)))
	(t (vector-push-extend flavor second-stack)
	   (dolist (c (flavor-components flavor))
	     (setf
	      undefined-includeds
	      (flavor-add-included
	       (get-flavor c t) i
	       components-stack second-stack
	       undefined-includeds)))
	   (dolist (incl (flavor-included-flavors flavor))
	     (setf
	      undefined-includeds
	      (flavor-add-included
	       (get-flavor incl t) i
	       components-stack second-stack
	       undefined-includeds)))))
  undefined-includeds)


;;; First we do a depth-first walk of the components.
;;; Into this list we insert all included flavors and their not-already-present
;;; components after the last flavor to include them.
;;; @#@# The order in which this is done is important: do we add includeds from
;;;  the end or from the beginning?
;;; Lastly, we add vanilla-flavor unless a components says not to.

(defun calculate-all-components (flavor undefined)
  (with-stacks (components-stack second-stack)
    (let ((undefined-flavors
	    (flavor-add-component flavor components-stack nil))
	  (undefined-includeds nil)
	  flavor-with-included
	  (length-components-stack (length components-stack))
	  )
      (dotimes (i length-components-stack)
	(let ((flav (aref components-stack  i  )))
	  (vector-push-extend flav second-stack)
	  (dolist (incl (flavor-included-flavors flav))
	    (if					;moe 1/3/85
	      (and
	       (setq flavor-with-included
		    (find incl components-stack
			  :from-end t		;moe 1/3/86
			  :test #'(lambda (incl c)
			(member  incl
				 (flavor-included-flavors c)))))
	       (equal flavor-with-included flav))	;moe 1/3/86
		(setq undefined-includeds
		    (flavor-add-included
		      (get-flavor incl)
		      i
		      components-stack second-stack
		      undefined-includeds))))))

      (unless (find-if #'(lambda (c) (not (flavor-has-vanilla-p c)))
		       second-stack)
	(vector-push-extend (get-flavor 'vanilla-flavor) second-stack))
      (cond ((car undefined-flavors)
             (funcall undefined (flavor-name flavor) undefined-flavors))
            (t (when (car undefined-includeds)
                 (warn "Undefined included flavors ignored - ~S."
		       undefined-includeds))
               (coerce second-stack 'list))))))

;;; We make one pass to get the ordered instance variables; any flavor
;;; that specifies ordered variables should list the same ones
;;; first as any other.
;;; Required variables (those with 'REQUIRED as a default) get
;;; replaced in the stack with the first real iv encountered.
;;; @#@# Generally the first iv to supply a default sets it?
;;; Ables flags (initable, settable, etc.) are ored together.


(defun calculate-instance-env (flavor)
  (with-stacks (ables-stack default-stack variables-stack)
    (setf (fill-pointer variables-stack) 0
	  (fill-pointer default-stack) 0
	  (fill-pointer ables-stack) 0)
    (fill default-stack 'unsupplied)
    (fill ables-stack 0)
    (dolist (flavor (flavor-all-components flavor))
      (let* ((env (flavor-method-env flavor))
	     (vec (iv-env-vector env)))
	(dotimes (i (length vec))
	  (let ((var (aref vec i))
		(temp nil))
	    (cond ((setq temp (position var variables-stack))
		   (if (eq 'unsupplied (aref default-stack temp))
                       (setf (aref default-stack temp)
                             (aref (method-env-defaults env) i)
                             (aref ables-stack temp)
                             (logior (aref (method-env-ables env) i)
                                     (aref ables-stack temp)))
		       ;;; make sure all ables logiored together.
		       ;;; bug 2029 fix.
		       (setf (aref ables-stack temp)
			     (logior (aref (method-env-ables env) i)
				     (aref ables-stack temp)))))
		  (t (vector-push-extend var variables-stack)
		     (vector-push-extend (aref (method-env-defaults env) i)
					 default-stack)
		     (vector-push-extend (aref (method-env-ables env) i)
					 ables-stack)))))))
    (make-instance-env :numordered 0
		       :vector (copy-seq variables-stack)
		       :defaults (copy-seq default-stack)
		       :ables (copy-seq ables-stack)
		       :required
		       (let (res)
			 (dotimes (i (length default-stack))
			   (when (eq 'required (aref default-stack i))
			     (push (aref variables-stack i) res)))
			 res))))

#| Original version with dead , incorrect code for ordered instance variables.

(defun calculate-instance-env (flavor)
  (with-stacks (ables-stack default-stack variables-stack)
    (let* ((ordered 0) (ovars '#()) oflavor temp)
      (dolist (flavor (flavor-all-components flavor))
        (let* ((env (flavor-method-env flavor))
               (newnum (method-env-numordered env))
               (newvars (iv-env-vector env))
               (diff (mismatch newvars ovars :end2 ordered :end1 ordered)))
          (macrolet
            ((diff ()
               '(if diff
                    (error "Ordered variable ~S in flavor ~S conflicts ~
                           with ordered variable ~S in flavor ~S."
                           (aref ovars diff) (flavor-name oflavor)
                           (aref newvars diff) (flavor-name flavor)))))
            (cond ((> newnum ordered)
                   (diff) (setq ovars newvars ordered newnum))
                  (t (diff))))))
      (setf (fill-pointer variables-stack) ordered
            (fill-pointer default-stack) ordered
            (fill-pointer ables-stack) ordered)
      (replace variables-stack ovars :end1 ordered)
      (fill default-stack 'unsupplied)
      (fill ables-stack 0)
      (dolist (flavor (flavor-all-components flavor))
        (let* ((env (flavor-method-env flavor))
               (vec (iv-env-vector env)))
          (dotimes (i (length vec))
            (let ((var (aref vec i)))
              (cond ((setq temp (position var variables-stack))
                     (if (eq 'unsupplied (aref default-stack temp))
                       (setf (aref default-stack temp)
                             (aref (method-env-defaults env) i)
                             (aref ables-stack temp)
                             (logior (aref (method-env-ables env) i)
                                     (aref ables-stack temp)))
		       ;;; make sure all ables logiored together.
		       ;;; bug 2029 fix.
		       (setf (aref ables-stack temp)
			     (logior (aref (method-env-ables env) i)
				     (aref ables-stack temp)))))
                    (t (vector-push-extend var variables-stack)
                       (vector-push-extend (aref (method-env-defaults env) i)
                                           default-stack)
                       (vector-push-extend (aref (method-env-ables env) i)
                                           ables-stack)))))))
      (make-instance-env :numordered ordered
                         :vector (copy-seq variables-stack)
                         :defaults (copy-seq default-stack)
                         :ables (copy-seq ables-stack)
                         :required
                         (let (res)
                           (dotimes (i (length default-stack))
                             (when (eq 'required (aref default-stack i))
                               (push (aref variables-stack i) res)))
                           res)))))

|#

;;; Defflavor

;;; %flavor-forms calculates the currently valid accessor forms
;;; (etc.?)  Later operation can figure out which ones are no longer
;;; valid.
(defmacro defflavor (flavor-name ivs components &rest options)
  "(flavor-name iv-list component-list . options)
  Refer to the flavor documentation for details."
  (format t "options: ~a~%" options)
  (when (and (find-class flavor-name nil)
	     (not (flavorp flavor-name)))
    (error "Flavor name ~S is not allowed." flavor-name))
  (%defflavor flavor-name ivs components options)
  `(progn
     (eval-when (:execute compile load)
            (%defflavor ',flavor-name ',ivs ',components ',options))
          ,.(%flavor-forms flavor-name)
          ',flavor-name))


;;; Constructs the list of defstruct-like accessor definitions.


;;; this macro is a fix for bug 2021.  It replaces slot-unbound-p which
;;; references instance variables by index, not by name. In %flavor-forms,
;;; slot-unbound-p was assuming that multiple occurences of an
;;; instance variable in various flavors which are combined have
;;; the same index (ie the variable is always in the same place
;;; in the option list to defflavor.)  So far, %flavor-forms
;;; is the only function in which use of slot-unbound-p resulted
;;; in a bug, but it may still be true in other places.  I didn't
;;; do a global replace of slot-unbound-p with instance-var-unbound-p
;;; as it is slower and more cumbersome.

(defmacro instance-var-unbound-p (instance iv)
  `(if (eq 'unbound ,iv)
       t
     (let* ((instance-vec (instance-env-vector
			   (instance-descriptor-env
			    (instance-descriptor ,instance))))
	    (index (position ,iv  instance-vec)))
       (when index
	 (slot-unbound-p ,instance index)))))

(defun %flavor-forms (flavor-name)
  (let* ((flavor (get-flavor flavor-name))
	 (iv-env (flavor-instance-env flavor))	;; 10/29/85 added by moe
	 (iv-vec (iv-env-vector iv-env))	;; 10/29/85 added by moe
	 (iv-defaults (instance-env-defaults iv-env))	;;10/29/85 added by moe
         (forms nil))
    (do ((inits nil)
         (i (1- (length iv-vec)) (1- i)))	;;10/29/85 changed by moe
        ((minusp i)
         (cons `(defmethod (,flavor-name internal-init) () (progn ,@inits))
               forms))
      (let ((def (aref iv-defaults i))		;;10/29/85 chamged by moe
            (var (aref iv-vec i)))		;;10/29/85 chamged by moe

        (unless (or (eq def 'required) (eq def 'unsupplied))	;;this is where the
          (push `(when (instance-var-unbound-p self ,var)               ;;init method is generated
                   (setq ,var ,def))		                ;;It wasn't doing one for the
                inits))				                ;;mixin.
	))))

(defun %defflavor (flavor-name ivs components options)
  (let* ((flavor (get-flavor flavor-name t))
         (iv-list (mapcar #'(lambda (x) (if (listp x) (car x) x)) ivs))
         (old-components (flavor-components flavor))
         (old-includeds (flavor-included-flavors flavor))
	 (old-env (flavor-method-env flavor))
	 (old-env-vec (when old-env (method-env-vector old-env)))
	 (old-env-defaults (when old-env (method-env-defaults old-env)))
         included-flavors
         (vanilla-p t)
         (changed 0)
         (abstract-p nil)
         (combinations '((internal-init :progn . :base-flavor-last)))
         changed-methods ordered required settable gettable initable
         env required-methods required-flavors default-plist
         init-keywords required-inits prefix args documentation)
    (dolist (opt options)
      (cond ((listp opt)
	     (setq args (cdr opt) opt (car opt))
	     ;; if null cdr use all ivars
	     (when (and (not args) (member opt *var-options*))
	       (setq args iv-list)))
            ((member opt *var-options*) (setq args iv-list))
            (t (setq args t)))
      (case opt
        (:method-order nil)
        (:required-instance-variables (setq required args))
        (:ordered-instance-variables (setq ordered args))
        (:settable-instance-variables (setq settable args))
        (:gettable-instance-variables (setq gettable args))
        (:initable-instance-variables (setq initable args))
        (:no-vanilla-flavor (setq vanilla-p nil))
        (:abstract-flavor (setq abstract-p t))
        (:accessor-prefix (setq prefix (symbol-name (car args))))
        (:required-methods (setq required-methods args))
        (:required-flavors (setq required-flavors args))
        (:included-flavors (setq included-flavors args))
        (:init-keywords (setq init-keywords args))
        (:default-init-plist (setq default-plist args))
        (:required-init-keywords (setq required-inits args))
	(:documentation (setq documentation args))
        (:method-combination
         (dolist (ordering args)
           (let ((combination (cons (car ordering) (cadr ordering))))
             (dolist (method (cddr ordering))
               (push (cons method combination) combinations)))))
        (t (error "Unknown defflavor option ~S." opt))))
    (setq env (make-env ivs ordered required settable gettable initable)
          prefix (or prefix (concatenate 'string (symbol-name flavor-name) "-")))

    (when (not (and (equal components old-components)
                    (equal included-flavors old-includeds)
                    (eq vanilla-p (flavor-has-vanilla-p flavor))
                    (flavor-defined-p flavor) ; If we're an undefined included.
		    (null options)
		    ;;; fix for bug 2018.
		    (equal old-env-vec (method-env-vector env))
		    (equal old-env-defaults (method-env-defaults env))
		    )) ;redo if options given. moe 12/17/85
      (setf (changed-components changed) t)
      (do-inheriting-flavors (i flavor)
        (rework-flavor i)
        (setf (changed-components (flavor-changed i)) t))
      (setf (flavor-included-flavors flavor) included-flavors
            (flavor-components flavor) components
            (flavor-has-vanilla-p flavor) vanilla-p)
      (dolist (c components)
	;; this may need fixing
        (pushnew flavor (flavor-dependents (get-flavor c )))))
    (setf (changed-required-flavors changed)
          (not (equal required-flavors (flavor-required-flavors flavor)))
          (changed-required-methods changed)
          (not (equal required-methods (flavor-required-methods flavor)))
          (changed-default-plist changed)
          (not (equal default-plist (flavor-default-plist flavor)))
          (changed-init-keywords changed)
          (not (equal init-keywords (flavor-init-keywords flavor)))
          (changed-required-inits changed)
          (not (equal required-inits (flavor-required-inits flavor))))
    ;; This has been done pretty lazily.
    (let* ((old-env (flavor-method-env flavor)))
      (cond ((null old-env)
	     (setf
	      (flavor-method-env flavor) env
	      (flavor-instance-env flavor) (calculate-instance-env flavor)))
            ((not (equalp (iv-env-vector env) (iv-env-vector old-env)))
             (setf (changed-iv-order changed) t
                   (changed-required-ivs changed) t
                   (changed-iv-inits changed) t)
	     (setf
	      (flavor-method-env flavor) env
	      (flavor-instance-env flavor) (calculate-instance-env flavor)))
            (t (let ((old-env-ables (method-env-ables old-env))
                     (env-ables (method-env-ables env)))
                 (dotimes (i (length env-ables))
                   (when (not (eq (ables-initable (aref env-ables i))
                                  (ables-initable (aref old-env-ables i))))
                     (setf (changed-iv-keywords changed) t)))))))

    (dolist (c (flavor-combinations flavor))
      (when (not (member c combinations :test #'equal))
        (pushnew (car c) changed-methods)))
    (dolist (c combinations)
      (when (not (member c (flavor-combinations flavor) :test #'equal))
        (pushnew (car c) changed-methods)))
    (when (and (not (flavor-abstract-p flavor))
               abstract-p
               (flavor-instantiated-p flavor)
               (not (flavor-wayward-p flavor)))
      (setf (flavor-wayward-p flavor) t)
      (format *error-output* "Instances of flavor ~S temporarily dissociated:~%~
              it is now an abstract flavor."
              (flavor-name flavor))
      (setf (flavor-abstract-p flavor) t)
      (freeze-instances (flavor-descriptor flavor)))
    (when (and (flavor-abstract-p flavor)
               (not abstract-p))
      (setq changed (logior %all-components-changed% changed))
      (setf (flavor-changed flavor) %all-components-changed%
            (flavor-abstract-p flavor) nil))
    (let ((defined-methods (flavor-methods flavor)))
      (dolist (s settable)
        (let ((method (get-name s)))
          (unless (method-find method :primary defined-methods)
            (let ((fn (flavor-function-name flavor-name method :primary)))
              (define-get-method fn s)
              (method-add method :primary fn defined-methods)
              (push method changed-methods))))
        (let ((method (set-name s)))
          (unless (method-find method :primary defined-methods)
            (let ((fn (flavor-function-name flavor-name method :primary)))
              (define-set-method fn s)
              (method-add method :primary fn defined-methods)
              (push method changed-methods)))))
      (dolist (g gettable)
        (let ((method (get-name g)))
          (unless (method-find method :primary defined-methods)
            (let ((fn (flavor-function-name flavor-name method :primary)))
              (define-get-method fn g)
              (method-add method :primary fn defined-methods)
              (push method changed-methods)))))
      (when (flavor-defined-p flavor)
        (let* ((ables (method-env-ables env))
               (vec (iv-env-vector env)))
          (dotimes (i (length vec))
            (let ((var (aref vec i)))
              (when (and (ables-settable (aref ables i))
                         (not (member var settable)))
                (let ((name (set-name var)))
                  (deletef :primary (method-types name defined-methods)
                           :key #'car)
                  (push name changed-methods)))
              (when (and (ables-gettable (aref ables i))
                         (not (member var gettable))
			 (not (member var settable)))	;moe 1/4/86
                (let ((name (get-name var)))
                  (deletef :primary (method-types name defined-methods)
                           :key #'car)
                  (push name changed-methods))))))))

    (macrolet ((doit ()
		 '(setf (flavor-changed flavor) changed
		   (flavor-method-env flavor) env
		   (flavor-combinations flavor) combinations
		   (flavor-required-methods flavor) required-methods
		   (flavor-required-flavors flavor) required-flavors
		   (flavor-prefix flavor) prefix
		   (flavor-default-plist flavor) default-plist
		   (flavor-init-keywords flavor) init-keywords
		   (flavor-required-inits flavor) required-inits
		   (flavor-documentation flavor) documentation
		   (flavor-defined-p flavor) t)))
      (cond ((not (and (zerop changed) (null changed-methods)))
             (with-stacks (affected)
               (do-inheriting-flavors (i flavor affected)
                 (rework-flavor i)
                 (setf (flavor-changed i) (logior changed (flavor-changed i)))
                 (if changed-methods (rework-methods i changed-methods)))
               (doit)
               (with-stacks (ordered)
                 (order-flavors affected ordered)
                 (dotimes (i (length ordered))
                   (cleanup-flavor (aref ordered i))))))
            (t (doit)))
      ;; prevent duplicates
      (unless (member (flavor-name flavor)  *all-flavor-names*)
	(push (flavor-name flavor) *all-flavor-names*))
      (deletef (flavor-name flavor) *undefined-flavor-names*))))
