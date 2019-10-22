;;; -*- Package: LUCID; Base: 10; Mode: LISP; Syntax: Common-lisp -*-
;;; ***************************************************************************
;;;
;;;        Copyright (C) 1985 by Lucid Inc.,  All Rights Reserved
;;;
;;; ***************************************************************************

(in-package "LUCID")


;;; Symbol macros are akin to labels or macrolet; the code for a
;;; replacement is in the same environment that the original symbol was
;;; in (including the symbol macros).  This is mostly because it's
;;; easier to implement.
;;;
;;; Newer version to deal with environments correctly.  Since there's no
;;; way to tell a macro with &environment in its arglist from other
;;; macros, we must always maintain the lisp system's view of the
;;; environment.  We can do this in a clumsy way by having the
;;; symbol-macro-let macro return somthing containing an internal macro
;;; that gets the environment where it appears and then deals with its
;;; body using that information.
;;;

(defvar *symbol-macro-environment* nil
  "Used to pass the expansion environment to the transforms.")

(defvar *symbol-macro-replacements* nil
  "Holds symbol-macro-let replacements during macroexpansion.")

(eval-when (eval compile)
(defmacro symbol-replaced-p (symbol)
  `(assoc ,symbol *symbol-macro-replacements*))

(defmacro symbol-replacement (replaced-p)
  `(cdr ,replaced-p))
)

;;; Binding functions.

(defun bind-symbol-macro (symbol expansion)
  (push (cons symbol expansion) *symbol-macro-replacements*))


(eval-when (eval compile)

(defmacro bind-non-macros (list &body forms);removed from following eval-when
                                            ;moe 12/4
                                            ;put back . 2/18/86 moe.
  `(let ((*symbol-macro-replacements*
          (remove-if #'(lambda (x) (member x ,list))
                     *symbol-macro-replacements*
                     :key #'car)))
     ,@forms))

(defmacro bind-non-macro (var &body forms)
  `(let ((*symbol-macro-replacements*
          (remove ,var *symbol-macro-replacements* :key #'car)))
     ,@forms))

)

;;;
;;; The macro itself.
;;;

(defmacro symbol-macro-let (bindings &rest body
			    &environment *symbol-macro-environment*)
  (let ((*symbol-macro-replacements* *symbol-macro-replacements*))
    (dolist (binding bindings)
      (bind-symbol-macro (car binding) (cadr binding)))
    `(progn ,@(mapcar #'symmac-replace body))))


;;; 
;;; Replacement functions.
;;;

;;; Macros and "symbol macros" get expanded, and the cycle repeats.
;;; Normal functions just get their arguments replaced on.
;;; Lambda calls get their bodies replaced as well as their args.
;;; Special forms have their own random ways of getting replaced on,
;;;  stored on their 'sym-mac-transform property.
;;;

(defvar *sym-mac-transforms* (make-hash-table))

(defun symmac-replace (form)
  (prog (temp)
    loop
    (cond ((and (symbolp form)
                (setq temp (symbol-replaced-p form)))
           (setq form (symbol-replacement temp))
           (go loop))
          ((atom form) (return form))
          ((not (symbolp (car form)))
           (let ((lambda (car form))
                 (args (cdr form)))
             (or (and (listp lambda) (eq (car lambda) 'lambda))
               (error "Illegal function object: ~A" lambda))
             (return `((lambda ,@(replace-lambda (cadr lambda) (cddr lambda)))
                       ,@(symmac-replace-list args)))))
          ((setq temp (gethash (car form) *sym-mac-transforms*))
           (return (funcall temp form)))
          ((and (boundp '*in-the-compiler*)
		*in-the-compiler*
		(compiler-macro-function (car form)
					 *symbol-macro-environment*))
	   (setq form (compiler-macroexpand-1 form *symbol-macro-environment*))
	   (go loop))
	  ((macro-function (car form) *symbol-macro-environment*)
	   (setq form (macroexpand-1 form *symbol-macro-environment*))
           (go loop))
          ((special-form-p (car form))
           (error "Symbol-macro-let internal error: untransformed special~
                  form ~A." (car form)))
          (t (return (cons (car form)
                           (mapcar #'(lambda (form) (symmac-replace form))
                                   (cdr form))))))))

(defun symmac-replace-list (forms)
  (mapcar #'symmac-replace forms))

;;; Takes a lambda-list and a list of forms to replace in that environment.
;;; Returns a list of the new arglist and forms.

(defun replace-lambda (lambda-list forms)
  (macrolet ((symmac-lambda-bind-var (var)
	       `(setq *symbol-macro-replacements*
		      (delete ,var *symbol-macro-replacements*
			      :key #'car))))
    (do* ((*symbol-macro-replacements* (copy-list *symbol-macro-replacements*))
          (list lambda-list (cdr list))
          (search '(&optional &rest &key &allow-other-keys &aux))
          (temp nil)
          (newlist nil))
         ((endp list)
          (cons (nreverse newlist) (symmac-replace-list forms)))
      (declare (list newlist))
      (let ((car (car list)))
        (cond ((setq temp (member car search))
               (setq search (cdr temp))
               (push car newlist))
              (t (case (car search)
                   ;; This is for the normal vars.
                   (&optional (symmac-lambda-bind-var car)
                              (push car newlist))
                   ;; This next clause is for the optionals.  Etc.
                   (&rest (cond ((symbolp car)
                                 (symmac-lambda-bind-var car)
                                 (push car newlist))
                                (t (cond ((null (cdr car))
                                          (push car newlist))
                                         (t (push `(,(car car)
                                                    ,(symmac-replace
                                                      (cadr car))
                                                    ,@(cddr car))
                                                  newlist)
                                            (if (not (null (cddr car)))
                                                (symmac-lambda-bind-var
                                                 (caddr car)))))
                                   (symmac-lambda-bind-var (car car))))) 
                   (&key (symmac-lambda-bind-var car)
                         (push car newlist))
                   (&allow-other-keys
                    (cond ((symbolp car)
                           (symmac-lambda-bind-var car)
                           (push car newlist))
                          (t (if (not (null (nth 1 car)))
                                 (push `(,(car car)
                                         ,(symmac-replace (cadr car))
                                         ,@(cddr car))
                                       newlist)
                                 (push car newlist))
                             (if (not (null (nth 2 car)))
                                 (symmac-lambda-bind-var (nth 2 car)))
                             (if (listp (car car))
                                 (symmac-lambda-bind-var (cadar car))))))
                   (&aux (error "~A in lambda-list after &allow-other-keys."
                                car))
                   ((nil) (cond ((symbolp car)
                               (symmac-lambda-bind-var car)
                               (push car newlist))
                              (t (push `(,(car car)
                                         ,(symmac-replace (cadr car)))
                                       newlist)
                                 (symmac-lambda-bind-var (car car)))))
                   )))))))


;;;
;;; The transforms.
;;;

;;; Can take a raw function, which gets applied to the form to be transformed.
;;; If it changes the environment, it should bind *symbol-macro-replacements*.

(eval-when (eval compile)

(defmacro defsymtrans (special-form args-or-function &body body)
  (let* ((dummy-name (make-symbol (symbol-name special-form)))
	 (fn (if body
		 (prog1
		   `(defun ,dummy-name ,args-or-function
		      (let ((*symbol-macro-replacements*
			      *symbol-macro-replacements*))
			,@body))
		   (setq args-or-function `#',dummy-name))
		 `())))
    `(progn ,fn
	    (setf (gethash ',special-form *sym-mac-transforms*)
		  ,args-or-function))))

)

(defun symmac-leave-first-arg (form)
  (list* (car form) (cadr form)
         (symmac-replace-list (cddr form))))

(defun symmac-progn-like (form)
  (cons (car form) (symmac-replace-list (cdr form))))

(defsymtrans quote #'identity)
(defsymtrans go #'identity)
(defsymtrans declare #'identity)

(defsymtrans eval-when #'symmac-leave-first-arg)
(defsymtrans block #'symmac-leave-first-arg)
(defsymtrans return-from #'symmac-leave-first-arg)
(defsymtrans the #'symmac-leave-first-arg)

(dolist (name '(and catch if locally multiple-value-call
		    multiple-value-list multiple-value-prog1 or
		    prog1 prog2 progn progv
		    return step throw time unless
		    unwind-protect when))
  (setf (gethash name *sym-mac-transforms*)
	#'symmac-progn-like))
	    
(defsymtrans function (form)
  (let ((lambdap (cadr form)))
    (cond ((symbolp lambdap) form)
          ((atom lambdap) (error "Illegal arg to FUNCTION - ~A." lambdap))
          ((eq 'lambda (car lambdap))
           `#'(lambda ,@(replace-lambda (cadr lambdap) (cddr lambdap))))
	  ((eq 'named-lambda (car lambdap))
	   `#'(named-lambda
	       ,(cadr lambdap)
	       ,@(replace-lambda (caddr lambdap) (cdddr lambdap))))
          (t (error "Strange thing following function: ~S" (car lambdap))))))

(defsymtrans tagbody (form)
  `(tagbody ,@(symmac-replace-tagbody (rest form))))

(defun symmac-replace-tagbody (forms)
  (do ((forms forms (cdr forms))
       (newforms '()))
      ((null forms) (nreverse newforms))
    (declare (list newforms))
    (if (symbolp (car forms))
        (push (car forms) newforms)
        (push (symmac-replace (car forms)) newforms))))

(defsymtrans setq (form)
  (cons 'setf (symmac-replace-list (cdr form))))

(defsymtrans psetq (form)
  (cons 'psetf (symmac-replace-list (cdr form))))

(defun symmac-replace-binding-body (form)
  (let* ((*symbol-macro-replacements* (copy-list *symbol-macro-replacements*))
	 (operator (first form))
	 (sequentialp (or (eq operator 'let*)
			  (eq operator 'do*)
			  (eq operator 'prog*)))
	 (bound '())
	 (reinits '())
	 (bindings
	   (mapcar
	     #'(lambda (binding)
		 (let ((name binding))
		   (prog1 (cond ((atom binding)
				 (push binding bound)
				 (push '() reinits)
				 binding)
				(t (setq name (car binding))
				   (push name bound)
				   (push (cddr binding) reinits)
				   `(,(car binding)
				     ,(symmac-replace (cadr binding)))))
			  (when sequentialp
			    (setq *symbol-macro-replacements*
				  (delete name *symbol-macro-replacements*
					  :key #'car))))))
	     (cadr form))))
    (Bind-non-macros bound
      (let* ((final
	       (if
		 (or
		    (eq operator 'do)
		       (eq operator 'do*)) ;moe 2/19/86
		   `(,(prog1 (symmac-replace-list (caddr form))
			     (setq form (cdr form))))
		   `()))
	     (body
	       (if (eq operator 'let)
		   (symmac-replace-list (cddr form))
		   (symmac-replace-tagbody (cddr form))))
	     (bindings-with-reinits
	       (mapcar #'(lambda (binding reinit)
			   (if (null reinit)
			       binding
			       (nconc binding
				      (symmac-replace-list reinit))))
		       bindings
		       (nreverse reinits)))) ; [r 3Dec85 added nreverse]
	`(,operator ,bindings-with-reinits ,@final ,@body)))))
	    

(defsymtrans let #'symmac-replace-binding-body)
(defsymtrans do #'symmac-replace-binding-body)
(defsymtrans prog #'symmac-replace-binding-body)
(defsymtrans let* #'symmac-replace-binding-body)
(defsymtrans do* #'symmac-replace-binding-body)
(defsymtrans prog* #'symmac-replace-binding-body)

(defsymtrans cond (form)
  `(cond ,@(mapcar #'(lambda (clause)
                       (symmac-replace-list clause))
                   (cdr form))))

(defsymtrans defun (form)
  (let ((name (cadr form))
        (args (caddr form))
        (body (cdddr form)))
    `(defun ,name ,@(replace-lambda args body))))

(defsymtrans multiple-value-bind (form)
  (let ((bindings (cadr form))
        (values (caddr form))
        (forms (cdddr form)))
    `(multiple-value-bind ,bindings ,(symmac-replace values)
       ,@(bind-non-macros bindings
                          (symmac-replace-list forms)))))

(defsymtrans multiple-value-setq (form)
  (pop form)
  (let* ((vars (pop form))
         (values (pop form))
         (pairs '())
         (gens (mapcar #'(lambda (var)
                           (let ((gen (gensym)))
                             (push gen pairs)
                             (push var pairs)
                             gen))
                       vars)))
    `(multiple-value-bind ,gens ,(symmac-replace values)
       ,(symmac-replace `(setf ,@pairs)))))


(defun symmac-flet-labels-macrolet (form)
  `(,(car form)
    ,(mapcar #'(lambda (single-function)
		 `(,(car single-function)
		   ,@(if (eq (car form) 'macrolet)
			 (cdr single-function)
			 (replace-lambda (cadr single-function)
					 (cddr single-function)))))
	     (cadr form))
     (compiler-let ((*symbol-macro-replacements*
		      ',*symbol-macro-replacements*))
       (symbol-macro-let ()
	 ,@(cddr form)))))

(defsymtrans flet #'symmac-flet-labels-macrolet)
(defsymtrans labels #'symmac-flet-labels-macrolet)
(defsymtrans macrolet #'symmac-flet-labels-macrolet)

(defsymtrans compiler-let (form)
  (destructuring-bind (bindings . body) (rest form)
    (let ((names (mapcar #'(lambda (x)
			     (if (consp x) (car x) x))
			 bindings))
	  (vals (mapcar #'(lambda (x)
			    (if (consp x) (eval (second x)) nil))
			bindings)))
      (progv names vals
	`(compiler-let ,bindings ,@(symmac-replace-list body))))))
