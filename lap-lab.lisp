



(in-package :lucid)

;; for the sake of investigating Lucid machinery, we use Sun 68K as
;; our example architecture.
(defun get-target-name ()
  'sun-68k)

(defmacro deflap-operand (name lambda-list &body body)
  `(defun (deflap-operand ,name ,(get-target-name)) (.form.)
     .form.				;in case not otherwise used
     ,@(if (null lambda-list)
		    body
		    `((destructuring-bind ,lambda-list (rest .form.)
					  ,@body)))))

(defun string-append (&rest strings)
  (apply #'concatenate 'simple-string (mapcar #'string strings)))

;;;DEFSTRUCT-SIMPLE-PREDICATE
;;;Defines a simple structure predicate which does not check for inclusions

(defun structurep (object)
  (typep object 'structure-object))

(defun append-symbols (&rest symbols)
  (intern (apply #'string-append symbols)))

(defmacro def-compiler-macro (name lambda-list &body body)
  `(defmacro (def-compiler-macro ,name ,(get-target-name))
       ,lambda-list ,@body))

(defmacro define-compiler-macro-alt (name arglist &body body)
  `(eval-when (:compile-toplevel :load-toplevel :exec)
     (lucid::def-compiler-macro ,name ,arglist ,@body)))

(defmacro defstruct-simple-predicate (structure-name &optional predicate-name)
  ;; Defines a simple structure predicate which does not check for
  ;; inclusions
  (unless predicate-name
    (setq predicate-name (append-symbols structure-name '-p)))
  `(progn
     (defun ,predicate-name (x)
       (and (structurep x)
	    (find-class ',structure-name nil)
	    (typep x ',structure-name))) 
     (define-compiler-macro ,predicate-name (x)
       (alexandria:once-only (x)
	 (and (structurep x)
	      (find-class ',structure-name nil)
	      (typep x ',structure-name))))))

;; Used primarily by array-optimize.lisp (and by the now-obsolete file
;; apollo-runtime.lisp)
(defmacro defstruct-runtime-slot-function (structure-name 
					   slot-name
					   argument-name)
  ;; Evaluates the defstruct slot accessors when a file is compiled so
  ;; that the accessors and setf methods created upon load can work
  ;; without the original defstruct definition being around at
  ;; runtime.
  (let ((full-name (append-symbols structure-name '- slot-name)))
    `(unless (fboundp ',full-name)
       (defun ,full-name (,argument-name)
	 (,full-name ,argument-name))
       (def-compiler-macro (,full-name user)  (,argument-name)
	 (subst ,argument-name '.dummy-name.
		 ',(swank::compiler-macroexpand-1 `(,full-name .dummy-name.))))
       (set-setf-method-expander
	  ',full-name #'expand-compiler-macro-setf-method)
       ',full-name)))


;; (defmacro defstruct-simple-predicate
;;     (structure-name &optional predicate-name)
;;   ;; (when (consp predicate-name)
;;   ;;   (setf predicate-name (car predicate-name)))
;;   (unless predicate-name
;;     (setf predicate-name (append-symbols structure-name '-p)))
;;   `(progn
;;      (defun ,predicate-name (x)
;;        (and (structurep x)
;; 	    #+lucid (eq (structure-type x) ',structure-name)))
;;      (def-compiler-macro ,predicate-name user) (x)
;;      (once-only (x)
;;        `(and (structurep ,x)
;; 	     (eq (structure-type ,x) ',',structure-name)))))
