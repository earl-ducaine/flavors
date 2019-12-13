


(in-package :flavors)

(defun send (instance message &rest args)
  (apply 'flavor-send instance message args))

(defun memq (x y) 
  (loop 
     (cond ((atom y) (return nil))
	   ((eq x (car y)) (return y)))
     (pop y)))

(defmacro do-place-spec ((key val place) &body body)
  ;; Iterate over all keyword-value specs in PLACE
  (let ((it-var (gensym)))
    `(do* ((,it-var ,place (cddr ,it-var))
	   (,key (first ,it-var) (first ,it-var))
	   (,val (second ,it-var) (second ,it-var)))
	 ((null ,it-var) nil)
       . ,body)))

(defun check-place-spec (place)
  (unless (and (listp place)
	       (evenp (length place)))
    (error "PLACE ~S is not a list of keywords and values" place))
  (do-place-spec (key val place)
    (unless (memq key '(:before :after :outside :inside))
      (error "~A is not a valid place keyword" key)))
  t)

(defun list-nreverse (x)
  (let ((previous '()))
    (nreconc x previous)))

;;; parse-body
(defun parse-body (body &optional environment (doc-string-allowed t))
 (do ((forms body (cdr forms))
      (declarations nil)
      (docstring nil)
      form)
     ((atom forms)
      (if (null forms)
	  (return (values forms (list-nreverse declarations) docstring))
	  (error "~S is an ill-formed body." body)))
   (setq form (car forms))
   (cond ((consp form)
	  ;; see if this form is a macro
	  ;; that needs to be expanded into a declare
	  (let ((temp t))
	    (loop (when (or (null temp)
			    (and (consp form)
				 ;; Don't expand SETFs since that won't
				 ;;  lead to declarations, only to grief.
				 (memq (car form) '(declare setf))))
		    (return))
		  (multiple-value-setq
		    (form temp)
		    (if (and (boundp '*in-the-compiler*)
			     *in-the-compiler*)
			(compiler-macroexpand-1 form environment)
			(macroexpand-1 form environment)))))
	  ;; once this is done, process the new form
	  (if (and (consp form) (eq (car form) 'declare))
	      (push form declarations)
	      (return (values forms (list-nreverse declarations) docstring))))
	 ((and (cdr forms) (stringp form) (null docstring) doc-string-allowed)
	  (setq docstring form))
	 (t (return (values forms (list-nreverse declarations) docstring))))))



;; This macro constructs a function of the form MAKE-FOO-BAR-ADVICE
;; which accepts as an argument the old definition of FOO and returns
;; a lexical closure which does the advice and then calls the old
;; definition
(defmacro defadvice-old
    ((advised-function name &optional place)
			      arglist &body body &environment env)
  (check-type advised-function symbol)
  (check-place-spec place)
  (let ((advisor-name
	 (make-symbol
	  (string-append "MAKE-" advised-function "-" name "-ADVICE")))
	(cont-sym (gensym)))
    ;; Find out decls, bind for possible macroexpand
    (multiple-value-bind (body-forms declares doc) 
	(let ((.advice-continuation. cont-sym)) 
	  (parse-body body env t))
      `(progn
	 (defun ,advisor-name (,cont-sym)
	   #'(lambda ,arglist
	       ,@(if doc (list doc))
	       ;; Declarations before compiler-let
	       ,@declares		
	       (compiler-let ((.advice-continuation. ',cont-sym))
		 ;; Let code know how to continue it
		 ,@body-forms)))
	 (install-advice ',advised-function ',name ',advisor-name ',place)))))

;; This macro constructs a function of the form MAKE-FOO-BAR-ADVICE
;; which accepts as an argument the old definition of FOO and returns
;; a lexical closure which does the advice and then calls the old
;; definition
(defmacro defadvice ((advised-function name)
		     arglist &body body &environment env)
  (check-type advised-function symbol)
  (let ((cont-sym (gensym)))
    ;; Find out decls, bind for possible macroexpand
    (multiple-value-bind (body-forms declares doc) 
	(parse-body body env t)
      `(sb-ext::encapsulate
	',advised-function ',name
	(lambda ,(cons 'function arglist)
	  ,@(if doc (list doc))
	  ;; Declarations before compiler-let
	  ,@declares		
	    ;; Let code know how to continue it
	    ,@body-forms)))))

;; (defadvice (type-of instance-type-of) (object) (flavor-type-of object))


;; note function of encapsulate must be of a sort that takes (function
;; &rest args) where function is the function being encapsulated, and
;; args are the arguments that would have been passed to function if
;; it had not been encapsulated.
;; (sb-ext::encapsulate function-or-name 'trace
;;                         (lambda (function &rest args)
;;                           (apply #'trace-call info function args)))
