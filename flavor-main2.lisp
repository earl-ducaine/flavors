;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: (FLAVORS (LISP FLAVOR-INTERNALS)) -*-
;;; ***************************************************************************
;;;
;;;        Copyright (C) 1985 by Lucid Inc.,  All Rights Reserved
;;;
;;; ***************************************************************************

(in-package "FLAVORS")

;;;
;;; This is the heart of flavors - the routine that calculates the various
;;; parts of a flavor in sequence.  Flavor-all-components is split off 
;;; so that it can be used in other places.
;;;


(defvar *cleanup-enable* t "If nil, cleanup is suppressed.")

(defmacro without-cleaning-flavors (&body forms)
  "Suppresses heavy flavors calculation inside the body.  Useful when
  defining a series of wrappers or something, especially if you have
  *flavor-compile-methods* set to T."
  `(progn (let ((*cleanup-enable* nil))
            ,@forms)
          ))


(defvar *flavors-compile* nil) ; If T, not just cleaning up.
;; Used by compiler-compile-flavors, so it can set the handlers itself.")
(defvar *dont-do-methods* nil) 

(defvar *uninstantiable* nil) ; (flavor why-string &rest why-args)
(defvar *inheritablep* nil) ; (flavor message) --> T, nil, or :redefine
(defvar *remap* nil) ; (flavor)
(defvar *definer* nil) ; (message form)
       (defvar *set-handlers* nil) ; (flavor name-stack fn-stack all-p)
;; Note: nil means no handler

(defvar *methods* nil) ; Assoc of name to assoc of slot to list of methods.
(defvar *description* nil) ; A vector of slot seqs, pushed onto by order-wrappers
(defvar *called-methods* nil) ; A stack that gets push-newed onto.
(defvar *message* nil) ; Used to pass message to the ordering functions.


;;;
;;; Stuff used in cleaning up.
;;; 


(defun dissociate-instances (flavor why &rest why-args)
  (cond ((and (flavor-instantiated-p flavor)
              (not (flavor-wayward-p flavor)))
         (setf (flavor-wayward-p flavor) t)
         (format *error-output*
                 "Instances of flavor ~S temporarily dissociated:~%"
                 (flavor-name flavor))
         (apply #'format *error-output* why why-args)
         (let ((descriptor (flavor-descriptor flavor)))
           (freeze-instances descriptor)))
        (*flavors-compile*
         (error "Could not instantiate flavor ~A~%~?"
                (flavor-name flavor)
                why
                why-args))
        (t (error "Illegal call to dissociate-instances in flavor ~A"
                  (flavor-name flavor)))))

(defun descriptor-set-handlers (descriptor names fn-stack all-p)
  (when descriptor
    (dotimes (i (length names))
      (cond ((aref fn-stack i)
             (handle-message (aref names i) descriptor (aref fn-stack i)))
            (t (unhandle-message (aref names i) descriptor))))
    (when all-p
      (do-handlers ((h fn) descriptor)
                   (declare (ignore fn))
		   fn
                   (unless (find h names)
                     (unhandle-message h descriptor))))))

;;; This should only get called if the flavor is instantiable, not wayward.
;;; This stuff should happen both at load-time and compile-time.

(defun flavor-set-handlers (flavor names fns all-p)
  (let* ((old-desc (flavor-descriptor flavor))
         (env (flavor-instance-env flavor))
         (waywardp (flavor-wayward-p flavor)))
    (cond ((null old-desc)
           (let ((new (make-instance-descriptor (flavor-name flavor)
                                                env 'default-handler)))
             (descriptor-set-handlers new names fns all-p)
             (setf (flavor-descriptor flavor) new)))
          ((or (and (flavor-instantiated-p flavor)
                    (not (equalp (instance-env-vector env)
                                 (instance-env-vector
                                  (instance-descriptor-env old-desc)))))
               waywardp)
           (let ((new-desc (make-instance-descriptor
                            (flavor-name flavor) env 'default-handler)))
             (do-handlers ((h fn) old-desc) (handle-message h new-desc fn))
             (descriptor-set-handlers new-desc names fns all-p)
             (resize-instances old-desc new-desc
                               #'(lambda (self)
                                   (send self ':internal-init)))
             (setf (flavor-descriptor flavor) new-desc
                   (flavor-wayward-p flavor) nil))
           (When waywardp
             (format *error-output*
                     "Instances of flavor ~S reunited with the flavor."
                     (flavor-name flavor))))
          (t (descriptor-set-handlers
              (flavor-descriptor flavor) names fns all-p)))))

(defvar *undefined-components*
  #'(lambda (f c) (error "Flavor ~S has undefined components - ~S." f c)))

(defun flavor-all-components (flavor)
  (when (or (changed-components (flavor-changed flavor))
            (null (flavor-all-components+ flavor)))
    (setf (flavor-all-components+ flavor)
          (calculate-all-components flavor *undefined-components*)
          (flavor-changed flavor) %all-components-changed%))
  (flavor-all-components+ flavor))


;;; First, get a stack of the methods we want to compile.
;;; Then do a pass to get the combination.
;;; Then do a pass to order the methods.
;;; (Note that we get the :base-flavor-last methods in the reverse order).
;;; Then go through the methods and generate the code.

(defun internal-cleanup-flavor
       (flavor &optional (really *cleanup-enable*)
               &aux all-components (*flavors-compile* *flavors-compile*))
  (unless really (return-from internal-cleanup-flavor nil))
  (if *flavors-compile*
      (if (flavor-compiled-p flavor) (setq *flavors-compile* nil)
          (setf (flavor-changed flavor)
                (logior %all-components-changed% (flavor-changed flavor))))
      (unless (flavor-compiled-p flavor)
        (return-from internal-cleanup-flavor nil)))
  (flush-method-cache)
  (setq all-components (flavor-all-components flavor))
  (when (changed-required-flavors (flavor-changed flavor))
    (cond ((flavor-abstract-p flavor))
          (t (dolist (c all-components)
               (let ((rflavors nil))
                 (dolist (f (flavor-required-flavors c))
                   (unless (member (get-flavor f) all-components)
		     ;change f to (get-flavor f). moe 12/30/85
		     (push f rflavors))) (when rflavors
                   (funcall *uninstantiable* flavor
                            "Additional required flavors: ~S." rflavors))))))
    (setf (changed-required-flavors (flavor-changed flavor)) nil))
  (when (let ((changed (flavor-changed flavor)))
          (or (changed-iv-order changed)
              (changed-required-ivs changed)
              (changed-iv-inits changed)))
    (let* ((new-env (flavor-instance-env flavor))
	   (req-iv (instance-env-required new-env)))
      (when req-iv
        (cond ((flavor-abstract-p flavor))
	      ;;; add this loop to check for required-ivs in a
	      ;;; flavor's components.Fix for bug 2102.
	      (t (let ((rivs nil)
		       (most-components (remove flavor all-components)))
		   (dolist (c most-components)
		     (dolist (ri req-iv)
		       (when (find ri (instance-env-vector
					    (flavor-instance-env c)))
			    (push c rivs))
		       ))
		   (when (not rivs)
		     (funcall *uninstantiable*
			      flavor "Required instance variables ~S."
			      (instance-env-required new-env)))))))
      (setf (changed-required-ivs (flavor-changed flavor)) nil)))
  (setf (changed-iv-order (flavor-changed flavor)) nil)
  (setf (changed-iv-inits (Flavor-changed flavor)) nil)

  (catch 'dont-do-methods
    (if *dont-do-methods* (throw 'dont-do-methods nil))
    (with-stacks (methods combinations functions *description* *called-methods*)
      (cond ((changed-all-methods (Flavor-changed flavor))
             (dolist (c all-components)
               (let ((str (flavor-methods c)))

                 (do-methods (m str)
                   (unless (find m methods)
                     (when (find :combined (get-method-types m str) :key #'car
                                 :test-not #'eq)
                       (vector-push-extend m methods)))))))
            ((flavor-changed-methods flavor)
             (let ((vec (flavor-changed-methods flavor)))
               (dotimes (i (length vec))
                 (let ((elt (aref vec i)))
                   (if (listp elt)
                       (dolist (e elt)
                         (unless (find e methods)
                           (vector-push-extend e methods)))
                       (unless (find elt methods)
                         (vector-push-extend elt methods))))))))
      (set-stack-length combinations (length methods))
      (set-stack-length functions (length methods))
      (flet
        ((defining-form (fn flavor description form)
           (declare (inline defining-form))
           `(progn (setf (get ',fn 'description) ',(copy-seq description))
                   (internal-define-method
                    ,fn ,(flavor-instance-env flavor) (&rest %combined-args)
                    ,(list form))
                   (method-add
                    ',*message* :combined ',fn
                    (flavor-methods (get-flavor ',(flavor-name flavor))))
		   )))
        (dolist (c all-components)
          (dotimes (i (length methods))
            (let* ((*message* (aref methods i))
                   (newc (cdr (assoc *message* (flavor-combinations c))))
                   oldc)
              (when newc
                (cond ((setq oldc (aref combinations i))
                       (if (not (equal newc oldc))
                           (error "Method combination conflict for method ~S."
                                  *message*)))
                      (t (setf (aref combinations i) newc)))))))
        (let ((*methods* nil))
          (dolist (c all-components)
            (let ((defined-methods (Flavor-methods c)))
              (dotimes (i (length methods))
                (let* ((*message* (aref methods i))
                       (type-assoc (method-types *message* defined-methods)))
                  (unless (null type-assoc)
                    (let ((comb (or (aref combinations i) *default-combination*)))
                      (funcall (combination-ordering (car comb)) (cdr comb)
                               type-assoc)))))))
          (dotimes (i (length methods))
            (let* ((*message* (aref methods i))
                   (comb (or (aref combinations i) *default-combination*)))
              (setf (fill-pointer *description*) 0)
              (funcall (combination-ordering (car comb)) (cdr comb) nil t)
              (let (inherit fn)
                (dolist (c all-components)
                  (setq fn (method-find *message* :combined (flavor-methods
							      c)))
                  (when (setq inherit
                              (and fn (equalp *description* (get fn 'description))
                                   (= (length (method-called-methods fn))
                                      (length *called-methods*))
                                   (every #'(lambda (m) (find m *called-methods*))
                                          (method-called-methods fn))
                                   (funcall *inheritablep* c *message*)))
                    (return nil)))
                (cond
                 ((or (eq inherit :redefine) (null inherit))
						;makes a combined method by
						;default. Why?
                  (unless fn (setq fn (flavor-function-name
                                       (flavor-name flavor) *message* :combined)))
                  (let ((form
                         (funcall (combination-mixer (car comb)) (cdr comb))))
		    (cond ((eq (car form) 'method-apply)
                           (setf (aref functions i) (cadr form)))
                          (form (funcall *definer* fn
					;changed by moe 11/5/85
					;if form is nil, no combined
					;method is needed
					 (defining-form fn flavor *description* form))
				(setf (aref functions i) fn)))))
                 (t (setf (aref functions i) fn))))))))
      (when (changed-required-methods (flavor-changed flavor))
        (cond ((flavor-abstract-p flavor))
              (t (let ((rmethods nil))
                   (dolist (c all-components)
                     (dolist (m (flavor-required-methods c))
		       ;; ***** r 17-Sep-85
		       (let ((pos (position m methods)))
			 (unless (or (and pos (aref functions pos))
				     (and (flavor-descriptor flavor)
					  (get-handler m
						       (flavor-descriptor flavor))))
                         (push m rmethods)))))
                   (when rmethods
                     (funcall *uninstantiable* flavor
                              "Additional required methods: ~S." rmethods)))))
        (setf (changed-required-methods (flavor-changed flavor)) nil))
      (unless (flavor-abstract-p flavor)
        (funcall *set-handlers* flavor methods functions	;unhandles
						                ;method here
                 (changed-all-methods (flavor-changed flavor))))))	
  (dealloc-tiny-stack (flavor-changed-methods flavor))
  (setf (changed-all-methods (Flavor-changed flavor)) nil)
  (setf (flavor-compiled-p flavor) t)
  (when  (changed-iv-keywords (flavor-changed flavor))
    (setf (flavor-instance-env flavor)
	  (calculate-instance-env flavor))	;update instance env here
    (let* ((env (flavor-instance-env flavor))   ;moe 12/19/85
           (vec (iv-env-vector env))           
           (ables (method-env-ables env))
           (keyword (find-package "KEYWORD"))
           res)
      (dotimes (i (length vec))
        (when (ables-initable (Aref ables i))
          (push (cons (intern (symbol-name (aref vec i)) keyword) i) res)))
      (setf (flavor-iv-keywords* flavor) res))
    (setf (changed-iv-keywords (flavor-changed flavor)) nil))
  (let* ((changed (flavor-changed flavor))
         (req-inits (changed-required-inits changed))
         (plist (changed-default-plist changed))
         (keywords (changed-init-keywords changed)))
    (when (or req-inits plist keywords)
      (let ((new-req-inits nil) (newplist nil) (newkeywords nil))
        (dolist (c all-components)
          (when req-inits
            (dolist (req-init (flavor-required-inits c))
              (pushnew req-init new-req-inits)))
          (when plist
            (do ((list (flavor-default-plist c) (cddr list)))
                ((endp (cdr list)))
              (when (eq 'foo (getf newplist (car list) 'foo))
                (push (cadr list) newplist)
                (push (car list) newplist))))
          (when keywords
            (dolist (key (flavor-init-keywords c))
              (pushnew key newkeywords))))
        (when req-inits
          (setf (flavor-required-inits* flavor) new-req-inits
                (changed-required-inits (flavor-changed flavor)) nil))
        (when plist (setf (flavor-default-plist* flavor) newplist
                          (changed-default-plist (flavor-changed flavor)) nil))
        (when keywords
          (setf (flavor-init-keywords* flavor) newkeywords
                (changed-init-keywords (flavor-changed flavor)) nil))))))


;;;
;;; The interface routines.
;;;

(defmacro touch (f stack result-stack touched)
  `(cond ((find ,f ,touched))
	 ((find ,f ,result-stack))
	 (t (touch-components ,f ,stack ,result-stack ,touched))))

(defun touch-components (f stack result-stack touched)
    (vector-push-extend f touched)
    (dolist (c (flavor-components f)) 
      (touch (get-flavor c) stack result-stack touched))
    (when (find f stack) (vector-push-extend f result-stack))
    (vector-pop touched))


;;; We try to clean up the components first so we can inherit their
;;; combined methods.  This works in all cases but where a loop
;;; is present in the structure, so we have to test whether we can inherit
;;; or not.
;;;
;;; We traverse the tree of components, keeping track of the current
;;; path in touched (thus preventing loops), and ignoring when we
;;; hit the current path again. 

(defun order-flavors (stack result-stack)
  (with-stacks (touched)
    (dotimes (i (length stack))
      (let ((thing (aref stack i)))
	(touch thing stack result-stack touched)))))

(defun cleanup-all-flavors ()
  "Optimizes the cleaning of dirty flavors, allowing effective sharing
  of combined methods."
  (with-stacks (ordered)
    (order-flavors *dirty-flavors* ordered)
    (dotimes (i (length ordered))
      (cleanup-flavor (Aref ordered i))))
  (setf (fill-pointer *dirty-flavors*) 0))

(defun cleanup-flavor (flavor)
  "Cleans the flavor."
  (when (symbolp flavor) (setq flavor (get-flavor flavor)))  
  (let ((*uninstantiable* #'dissociate-instances)
        (*inheritablep* #'message-clean-p)
        (*definer* #'(lambda (name form)
                       (declare (ignore name))
		       name
                       (if *flavor-compile-methods*
                           (funcall (compile nil `(lambda () ,form)))
                           (eval form))))              
        (*set-handlers* #'flavor-set-handlers))
    (internal-cleanup-flavor flavor)))


;;; Propogates methods-need-work and then cleans up.
;;; Does nil message and do-dependents t mean all of everybody's methods,
;;; or just those inherited from the original?

(defun recompile-flavor (flavor-name &optional message (do-dependents t))
  "Use this when something compiled into a combined method has changed,
  for instance a macro used by a wrapper and hence compiled into the 
  combined method."
  (let ((flavor (get-flavor flavor-name))
        (*uninstantiable* #'dissociate-instances)
        (*inheritablep* #'message-clean-p)
        (*definer* #'(lambda (name form)
                       (declare (ignore name))
		       name
                       (if *flavor-compile-methods*
                           (funcall (compile nil `(lambda () ,form)))
                           (eval form))))              
        (*set-handlers* #'flavor-set-handlers))
    (cond (do-dependents
           (with-stacks (flavors ordered)
             (do-inheriting-flavors (flavor flavor flavors)
               (rework-flavor flavor)
               (if (null message)
                   (setf (changed-all-methods (flavor-changed flavor)) t)
                   (rework-methods flavor message)))
             (order-flavors flavors ordered)
             (dovec (o ordered) (internal-cleanup-flavor o))))
          ((null message)
           (rework-flavor flavor)
           (setf (changed-all-methods (flavor-changed flavor)) t)
           (internal-cleanup-flavor flavor))
          (t (rework-flavor flavor)
             (rework-methods flavor message)
             (internal-cleanup-flavor flavor)))))

(Defvar *flavors-in-compiler* nil)

;(defmacro compile-flavor-methods (&rest flavors)
;  `(progn (eval-when (eval) (setq *flavors-in-compiler* nil))
;          (eval-when (compile) (setq *Flavors-in-compiler* t))
;          (compile-flavors-aux ,flavors)
;          nil))

(defmacro compile-flavor-methods (&rest flavors);moe 1/29/86
  `(progn 
       (eval-when (compile load)
	 (compiler-compile-flavors . ,flavors))
       (eval-when (eval)
	 (dolist (flavor ',flavors)
	   (compile-flavor flavor)))))


(eval-when (eval compile)

(defmacro compile-flavors-aux (flavors)
  (if *flavors-in-compiler*
      `(compiler-compile-flavors ,@flavors)
      `(mapcar 'compile-flavor ',flavors)))

)

(defun compile-flavor (flavor &optional (*flavor-compile-methods*
                                         *flavor-compile-methods*))
  "Makes a non-abstract flavor ready for instantiation, and an abstract
  flavor sharable with respect to combined methods.  If you're doing this
  for a file, use compiler-compile-flavors instead."
  (let ((*flavors-compile* t))
    (cleanup-flavor flavor)))

(defmacro compiler-compile-flavors (&rest flavor-names)
  "Just like compile-flavors, only it includes the definitions of all
  combined methods - compile-flavors doesn't redefine methods it can
  find in the runtime environment.  This way, you're guaranteed to have
  all the functions you expect when you load the file.
  Uses the cleanup machinery, so it cleans up affected flavors first."

  (with-stacks (stack ordered defined-methods)
    (dolist (f flavor-names) (vector-push-extend (get-flavor f) stack))
    (order-flavors stack ordered)
    (dotimes (i (length ordered))
      (cleanup-flavor (aref ordered i)))
    (setf (fill-pointer ordered) 0) 
    (dolist (name flavor-names)
      (let ((flavor (get-flavor name)))
        (setf (changed-all-methods (flavor-changed flavor)) t)))
    (let* ((forms nil)
           (*uninstantiable* #'dissociate-instances)
           (*inheritablep*
            #'(lambda (flavor message)
                (cond ((member (flavor-name flavor) flavor-names) t)
                      ((message-clean-p flavor message) :redefine)
                      (t nil))))
           (*definer* #'(lambda (name form)
                          (unless (find name defined-methods)
                            (push form forms)
                            (vector-push-extend name defined-methods))))
           (*set-handlers*
            #'(lambda (flavor name-stack fn-stack all-p)
                (declare (ignore all-p))
		all-p
                (push `(flavor-set-handlers (get-flavor ',(flavor-name flavor))
                         ',(copy-seq name-stack) ',(copy-seq fn-stack) t)
                      forms)))
           (*flavors-compile* t))
      (order-flavors stack ordered)
      (dotimes (i (length ordered))
        (internal-cleanup-flavor (aref ordered i) t))
      `(progn (eval-when (load)
                (let ((*dont-do-methods* t))
                  (mapc #'compile-flavor ',flavor-names)))
              (eval-when (compile load)
                ,@(nreverse forms))))))
;;;
;;; Combination stuff.
;;;

;;; These macros refer to the specials of the previous section.

(eval-when (eval compile)

(defmacro get-slot (message slot)
  `(my-assoc ,message (my-assoc ,slot *methods*)))

)

(defun method-list (slot-name)
  "Takes the slot-name given in order-methods.  Returns the list of
  method-function-names resulting from the ordering.  For a :primary
  ordered slot, this will be a list of one function."
  (get-slot *message* slot-name))

(defun call-methods (slot-name-or-list)
  "Takes either a slot-name or a list of method function names.  Returns a list
  of forms that call them."
  (mapcar #'(lambda (x) `(method-apply ,x %combined-args))
          (if (listp slot-name-or-list) slot-name-or-list
              (get-slot *message* slot-name-or-list))))

(defun wrapper-mix (slot-name-or-list form)
  "Wraps the form with the wrappers (and whoppers) of the given types. 
  (See defwrapper, defwhopper.)  The ordering of the wrapper functions
  is the reverse of the order in which the wrappings will be encountered
  at runtime."
  (let ((list (if (atom slot-name-or-list) (get-slot *message* slot-name-or-list)
                  slot-name-or-list)))
    (dolist (l list)
      (setq form (funcall l form)))
    form))


(eval-when (eval compile)

(defmacro defcombination-ordering (name (arg) &body body)
  "Should use ORDER-METHODS or ORDER-WRAPPERS (almost exclusively) to
  order the method types of a method into named slots.
  Keep in mind that the code may be executed many times, depending upon the
  implementation."
  `(defun ,name (,arg type-assoc &optional reverse-and-describe)
     (declare (ignore ,arg))
     (dolist (cons (if reverse-and-describe '((foo . bar)) type-assoc))
       (let ((type (car cons))
             (method (cdr cons)))
         ,@body))))

(defmacro order-methods (order slot types)
  "Slot and types are evaled.  Slot-name is the name used in message-list, etc.
  Order is one of :base-flavor-last, :primary, or :base-flavor-first,
  as described in the documentation.  The method-types in the list are added
  in order to the list of methods in the slot."
  (case order
    (:primary `(if reverse-and-describe
                   (let ((thing (car (get-slot *message* ,slot))))
                     (unless (find thing *called-methods*)
                       (vector-push-extend thing *called-methods*)))
                   (when (member type ,types)
                     (unless (get-slot *message* ,slot)
                       (push method (get-slot *message* ,slot))))))
    (:base-flavor-first `(if reverse-and-describe
                             (dolist (m (get-slot *message* ,slot))
                               (unless (find m *called-methods*)
                                 (vector-push-extend m *called-methods*)))
                             (when (member type ,types)
                               (push method (get-slot *message* ,slot)))))
    (:base-flavor-last `(if reverse-and-describe
                            (dolist (m (nreversef (get-slot *message* ,slot)))
                              (unless (find m *called-methods*)
                                (vector-push-extend m *called-methods*)))
                            (when (member type ,types)
                              (push method (get-slot *message* ,slot)))))))

(defmacro order-wrappers (order slot types)
  "Just like order-methods, except remembers the list of wrapper functions
  along with the list of methods."
  `(cond (reverse-and-describe
          ,(if (eq order :base-flavor-last)
               `(nreversef (get-slot *message* ,slot)))
          (vector-push-extend (get-slot *message* ,slot) *description*))
         (t (order-methods ,order ,slot ,types))))


(defmacro defcombination (name ordering-fn (order-arg) &body body)
  "The order-arg is bound to the argument of the combination name.
  the ordering-fn is used to order the methods of the method we're combining
  (see defcombination-ordering).  The body of this form should produce
  the actual combined method, using the functions METHOD-LIST, CALL-METHODS,
  CALL-METHOD, AND WRAPPER-MIX to access the slots of the ordered methods.
  The system will optimize only bare method-calls, not those surrounded
  by AND, etc."
  `(make-combination ',name ',ordering-fn
                     #'(lambda (,order-arg)
			 (declare (ignore ,order-arg))
			 ,@body)))

)

;;;
;;; Random user-level functions
;;;

(defun symeval-in-instance (instance symbol &optional (no-error-p nil)
				     (unbound nil))
  "If no-error-p is non-nil, unbound ivs will simply return the value of
  the second optional. Ivs not present in the environment will signal an error
  unless no-error-p is non-nil."
  (let* ((vec (iv-env-vector (instance-descriptor-env
                              (instance-descriptor instance))))
         (pos (position symbol vec)))
    (cond ((null pos)				;moe 12/18/85
	   (if no-error-p nil
	       (error "Instance variable ~S not found in instance ~S."
                  symbol instance)))
          ((slot-unbound-p instance pos)
           (if unbound unbound
               (error "Instance variable ~S unbound." symbol)))
          (t (%instance-ref instance (1+ pos))))));moe 1/11/86
						;emulate old instance-ref

(defun set-in-instance (instance symbol value)
  "An error is signalled if the symbol is not an instance variable in the given
  instance."
  (let* ((vec (iv-env-vector (instance-descriptor-env
                              (instance-descriptor instance))))
         (pos (position symbol vec)))
    (cond ((null pos)
           (error "Instance variable ~S not found in instance ~S."
                  symbol instance))
          (t (setf (%instance-ref instance (1+ pos)) value)))))	;moe 1/11/86
						;emulate old instance-ref with
						;1+ 
(defun get-handler-for (object message)
  "Returns the function that handles the given message."
  (get-handler message object)) 

(eval-when (eval compile)

(defmacro lexpr-funcall-self (message &rest arguments)
  "Supplied for compatibility with Symbolics Flavors."
  `(apply #'send self ,message ,@arguments))

(defmacro funcall-self (message &rest arguments)
  "Supplied for compatibility with Symbolics Flavors."
  `(send self ,message ,@arguments))

)

(defun flavor-allowed-init-keywords (flavor-name)
  (let ((flavor (get-flavor flavor-name))
        (res nil))
    (dolist (flav (remove (get-flavor 'vanilla-flavor)
			  (flavor-all-components flavor)))
      (compile-flavor flav)                             
      (setf res (append (flavor-init-keywords flav) res))
      (dolist (key-list (flavor-iv-keywords* flav))	
	(setf res (cons (car key-list) res))))       
    (delete-duplicates                               
      (sort res #'string-lessp :key #'symbol-name))))	

(defun flavor-allows-init-keyword-p (flavor-name keyword)
  "Returns the flavor that provides this init option, or NIL if none.
   If it is an iv keyword, it returns the composite flavor name"
  (let ((flavor (get-flavor flavor-name))
	(iv-list ()))
    (dolist (flav (flavor-all-components flavor))               
      (dolist (iv-keyword-list  (flavor-iv-keywords* flavor))	
	(push (car iv-keyword-list) iv-list))
      (if (or (find keyword (flavor-init-keywords flav))
	      (find keyword iv-list))
	  (return-from flavor-allows-init-keyword-p (flavor-name flav))))))

(defun make-instance (flavor-name &rest init-plist)
  "Compiles the flavor and makes sure it's initable, inits any initable
  instance variables from the init-plist and inits others if inits given,
  then sends :INIT with the plist if it's handled.  Returns the new instance."
  (typecase init-plist
    (list)
    (symbol (setq init-plist (symbol-plist init-plist)))
    (t (error "Init-plist not list or symbol - ~S." init-plist)))
  (let* ((flavor (get-flavor flavor-name)))
    (when (flavor-abstract-p flavor)
      (error "Attempt to instantiate an abstract flavor: ~S." flavor-name))
    (let ((*flavors-compile* t))
      (cleanup-flavor (get-flavor flavor-name)))
    (with-stacks (unused-properties used-properties)
      (let* ((desc (flavor-descriptor flavor))
             (self (instantiate-instance-descriptor desc))
             (new-plist nil))
        ;; Scan the init-plist for variable inits.  If it's not a variable
        ;; init, make sure it's in the allowed-keywords sequence.
        (do ((plist init-plist (cddr plist)))
            ((endp plist))
          (if (plusp (length unused-properties))
              (if (not (find (car plist) (flavor-init-keywords* flavor)))
                  (vector-push-extend (car plist) unused-properties))
              (let ((iv (cdr (assoc (car plist) (flavor-iv-keywords* flavor)))))
                (cond (iv (when (slot-unbound-p self iv)
                            (setf (%instance-ref self (1+ iv)) (cadr plist))
			    ;moe 1/11/86 emulate old instance-ref with 1+
                            (vector-push-extend (car plist) used-properties)))
                      ((find (car plist) (flavor-init-keywords* flavor))
                       (when (eq 'frob (getf new-plist (car plist) 'frob))
                         (vector-push-extend (car plist) used-properties)
                         (push  (cadr plist) new-plist);moe remove eval 10/5/87
                         (push (car plist) new-plist)))
                      (t (vector-push-extend (car plist) unused-properties))))))
        (do ((plist (flavor-default-plist* flavor) (cddr plist)))
            ((endp plist))
          (if (plusp (length unused-properties))
              (if (not (find (car plist) (flavor-init-keywords* flavor)))
                  (vector-push-extend (car plist) unused-properties))
              (let ((iv (cdr (assoc (car plist) (flavor-iv-keywords* flavor)))))
                (cond (iv (when (slot-unbound-p self iv)
                            (setf (%instance-ref self (1+ iv)) (eval (cadr
			    plist))) ;moe 1/11/86
                            (vector-push-extend (car plist) used-properties)))
                      ((find (car plist) (flavor-init-keywords* flavor))
                       (when (eq 'frob (getf new-plist (car plist) 'frob))
                         (push (eval(cadr plist)) new-plist) ;moe 10/8/87
                         (push (car plist) new-plist)
                         (vector-push-extend (car plist) used-properties)))
                      (t (vector-push-extend (car plist) unused-properties))))))
        (if (plusp (length unused-properties))
            (error "Unknown init keywords for make-instance: ~S."
                   (coerce unused-properties 'list)))
        (send self 'internal-init)
        (let (required)
          (dolist (req (flavor-required-inits* flavor))
            (unless (find req used-properties)
              (push req required)))
          (if required (error "Additional required keywords for flavor ~S: ~S."
                              flavor-name required)))
        (send self :send-if-handles :init new-plist)
        self))))


(defun undefflavor (name)
  "Permanently dissociates the flavor's instances and undefines the flavor."
  (let ((flavor (get-flavor name)))
    (when (flavor-defined-p flavor)
      (with-stacks (affected)
        (do-inheriting-flavors (i flavor affected)
          (rework-flavor i)
          (setf (changed-components (flavor-changed i)) t))
        (dissociate-instances flavor "Flavor has been undefflavored.")
        (setf (flavor-methods flavor) (make-method-structure)
              (flavor-descriptor flavor) nil
              (flavor-compiled-p flavor) nil
              (flavor-defined-p flavor) nil)
        (deletef name *all-flavor-names*)
        (dolist (c (flavor-components flavor))
          (deletef name (flavor-dependents (get-flavor c))))
        (with-stacks (ordered)
          (order-flavors affected ordered)
          (dotimes (i (length ordered))
            (cleanup-flavor (aref ordered i))))))
    name))

(defmacro undefmethod ((flavor type &optional (message nil mp)))
  "Undoes the effect of the corresponding defmethod, defwrapper (use type :wrapper)
  , or whopper (type :whopper)."
  (if (not mp) (setq message type type :primary))
  `(%undefmethod ',flavor ',message ',type))

(defun %undefmethod (flavor-name message type)
  (let* ((flavor (get-flavor flavor-name))
	 (fl-methods (flavor-methods flavor))
	 (meth-types (method-types message fl-methods))
	 (method-exists-p (member type meth-types :key #'car))
	                                        ;does method
						;exist for this flavor?

	 (methods-remaining-p t ))		;assume there are some method
						;types remaining for this
						;message 
					
    (if method-exists-p
	(setq methods-remaining-p
	      (deletef type (method-types message fl-methods)
		       :key #'car))
	(error "Method ~S not defined for flavor ~S." message
	       flavor-name))		        ;if deletef returns nil, there
						;are no more remaining method
						;types for this message.
						;In this case, %undefmethod
						;needs to update the method
						;info in the method-structure.
						;If deletef is non-nil, then
						;deletef handles the update.
    (when (and method-exists-p (null methods-remaining-p))
      (setf (method-structure-methods fl-methods)
	    (delete message (method-structure-methods fl-methods)))
      (setf (method-structure-types fl-methods)
	    (delete nil (method-structure-types fl-methods))))
    (recompile-flavor (flavor-name flavor) message)	
						;recompile-flavor propagates
						;the changes in
						;method-structure to the
						;message hashtable
    )
  message)

;;; Defines a function that takes the argument name and a a rest arg of the
;;; body, and that does the transformation for that wrapper, returning the 
;;; new form.

(defmacro defwrapper ((flavor type &optional (method nil mp))
                      (args . passed-body) &body body)
  "Wrappers are sort of like macros, getting expanded into the combined method
  for flavors that inherit it.  The args is either ignore or a list that
  is destructure-bound at send-time to the method's argument-list.  The
  passed-body gets bound at 'expansion time' to the 'inside' of the wrapper."
  ;;
  (if (not mp) (setq method type type :wrapper))
  (if (eq args 'ignore) (setq args nil))
  (let* ((name (flavor-function-name flavor method type))
         (new-defun `(defun ,name (,passed-body)	;took out &rest
						;put in call to list 11/15/85 moe
		       (setq ,passed-body (list ,passed-body))	
                       (let ((form (progn ,@body)))
                         `(mydlet ((,',args %combined-args))
                            ,form)))))
    `(progn
      (eval-when (compile eval load)
        ,new-defun)
      (eval-when (compile eval)
        (let ((new-defun ',new-defun)
              (old-defun (get ',name 'old-defun))
              (new-hash nil))
          (when (or (and (null old-defun)
                         (let ((hash (get ',name 'sxhash)))
                           (or (null hash)
                               (not (= hash (setq new-hash (sxhash new-defun)))))))
                    (not (equal new-defun old-defun)))
            (method-add ',method ',type ',name
                        (flavor-methods (get-flavor ',flavor)))
            (recompile-flavor ',flavor ',method)
            (setf (get ',name 'old-defun) new-defun)
            (if new-hash
                (setf (get ',name 'sxhash) new-hash)))))
      (eval-when (load)
        (let ((hash (get ',name 'sxhash))
              (new (sxhash ,new-defun)))
          (when (or (null hash) (not (= hash new)))
            (method-add ',method ',type ',name
                        (flavor-methods (get-flavor ',flavor)))
            (recompile-flavor ',flavor ',method)
            (setf (get ',name 'sxhash) new))))
      ',method)))


(defmacro defmethod ((flavor-name type &optional (method nil methodp)
				  (case-key nil case-keyp)) ;moe
                     args &body forms)
  "Refer to the flavor documentation for details."
  ;; case requires type to specify :case
  (cond ((not methodp)
	 (psetq method type type :primary))
	((eq type :case)
	 ;; if :case type, then set the type to :primary
	 ;; the :case key gets stored on the plist of the
	 ;; flavor-function-name by method-add
	 (unless case-keyp (error "~% must supply case key type"))
	 (setq type :primary)))
  (let* ((flavor (get-flavor flavor-name))
	 (method-list nil)
	 (function-name (if case-keyp
			    (prog1
			      (flavor-function-name flavor-name method
						    case-key type)
			      (setq method-list (cons method case-key)))
			    (flavor-function-name flavor-name method type))))
    (unless (flavor-defined-p flavor)
      (error "Flavor ~S not defined." flavor-name))
    (multiple-value-bind (docs forms) (extract-doc-and-declares forms)
      `(eval-when (eval load compile)
         (internal-define-method ,function-name ,(flavor-instance-env flavor)
                                 ,args (,@docs (block ,method ,@forms)))
         (%defmethod ',flavor-name (or ',method-list ',method)
		     ',type ',function-name)))))

(defun %defmethod (flavor-name method type function-name)
  (if (method-add method type function-name
                  (flavor-methods (get-flavor flavor-name)))
      (recompile-flavor flavor-name method))
  method)


;;; The continuation is a lambda closed in the instance environment.
;;; The type used in the defwhopper is the type of the wrapper (default :whopper).
;;; The whopper itself is a method of the flavor - it (and hence the name)
;;; depends upon the flavor it's defined for, the method and type.
;;; (so that all inheriting flavors have it).

(defmacro defwhopper ((name &rest method-and-type) args &body body)
  "Whoppers are to wrappers what functions are to macros.  Whoppers
  are functions that call continue-whopper, lexpr-continue-whopper,
  or continue-whopper-all with the desired arguments to continue with the
  wrapped code."
  (setq method-and-type (nreverse method-and-type))
  (let* ((method (pop method-and-type))
         (type (or (pop method-and-type) :whopper))
         (whopper-method (flavor-function-name name method type "WHOPPER")))
    `(progn (defmethod (,name ,whopper-method)
                       (%continuation %combined-args ,@args)
              ,@body)
            (defwrapper (,name ,type ,method) (nil . wrapper-body)
              `(apply #'send self ',',whopper-method
                      #'(lambda (%arglist &rest %combined-args)
                          (if (listp %arglist) (setq %combined-args %arglist))
                          ,@wrapper-body)
                      %combined-args
                      %combined-args)))))

(defmacro continue-whopper-all ()
  "See defwhopper.  Continues the combined method with all the passed arguments
  (efficiently)."
  `(funcall %continuation %combined-args))

(defmacro continue-whopper (&rest args)
  "See defwhopper and continue-whopper-all.  Continue-whopper lets you continue
  the combined method with different arguments."
  `(funcall %continuation t ,@args))

(defmacro lexpr-continue-whopper (&rest args)
  "See defwhopper."
  `(apply %continuation t ,@args))


;;; Redefine flavors-available-p
(in-package 'system)
(export 'flavors-available-p)
(defun flavors-available-p ()
  t)
