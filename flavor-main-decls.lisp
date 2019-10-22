;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: (FLAVORS (LISP FLAVOR-INTERNALS)) -*-
;;; ***************************************************************************
;;;
;;;        Copyright (C) 1985 by Lucid Inc.,  All Rights Reserved
;;;
;;; ***************************************************************************

(in-package "FLAVORS")

(proclaim '(function flavor-all-components (t) t))
(proclaim '(function order-flavors (t t) t))
(proclaim '(function cleanup-flavor (t) t))

;;;
;;; Random utilities.
;;;

(defmacro mydlet (bindings &body body)
    (let ((runtime nil))
      (dolist (binding bindings)
        (unless (null (car binding))
          (let ((list (gensym)))
            (push `(,list ,@(cdr binding)) runtime)
            (mapcar #'(lambda (var) (push `(,var (pop ,list)) runtime))
                    (car binding)))))
      (cond ((null runtime) `(progn ,@body))
            (t `(let* ,(nreverse runtime)
                  ,@body)))))

(defun private-structure-printer (object stream depth)
  (declare (ignore depth))
  depth
  (format stream "#<~A ~X>" (type-of object) (lucid::%pointer object)))


;;; Boolean variables shouldn't take up 32 bits.
;;; Syntax and semantics like defstruct.

(eval-when (eval compile)

(define-setf-method logbitp (index place &environment env)
  (multiple-value-bind (temps vals stores store-form access-form)
      (get-setf-method place env)
    (let ((itemp (gensym))			; temp var for index
	  (store (gensym))			; temp var for result
	  (stemp (first stores)))		; temp var for place
      (values
	;; The list of temporary variables
	(cons itemp temps)
	;; The list of value forms
	(cons index vals)
	;; The list of store variables
	(list store)
	;; The store form
	`(let ((,stemp (if ,store
			   (logior (ash 1 ,itemp) ,access-form)
			   (logandc1 (ash 1 ,itemp) ,access-form))))
	   ,store-form
	   ,store)
	;; The access form
	`(logbitp ,itemp ,access-form)))))

(defmacro defbits (str-name &rest names)
  (do ((i 0 (1+ i))
       (names names (cdr names))
       (res nil))
      ((null names) `(progn ,@res))
    (push `(defmacro ,(intern (concatenate 'string (symbol-name str-name)
					   "-" (symbol-name (car names))))
		     (thing)
	     `(logbitp ,,i (the fixnum ,thing)))
	  res)))

)

;;; Assoc with a nicer setf method.

(defsubst my-assoc (key list)
  "Just like simple assoc, but has a nice setf method."
  (cdr (assoc key list)))

(define-setf-method my-assoc (key list)
  (multiple-value-bind (temps vals stores store-form access-form)
      (get-setf-method list)
    (let ((ktemp (gensym))
	  (list (gensym))
	  (assoc (gensym))
	  (store (gensym))
	  (stemp (first stores)))
      (values `(,ktemp ,.temps ,list ,assoc)
	      `(,key ,.vals ,access-form (assoc ,ktemp ,list))
	      (list store)
	      `(if ,assoc
		   (setf (cdr ,assoc) ,store)
		   (let ((,stemp (cons (cons ,ktemp ,store) ,list)))
		     ,store-form
		     ,store))
	      `(cdr ,assoc)))))

(define-modify-macro nreversef () nreverse)

(eval-when (eval compile)

(defmacro deletef (item sequence &rest keywords)
    `(setf ,sequence (delete ,item ,sequence ,@keywords)))

(defmacro dovec ((var vec) &body body)
    `(let ((%vec ,vec))
       (dotimes (%i (length %vec))
         (let ((,var (aref %vec %i)))
           ,@body))))

;;; Stacks are just dynamically allocated vectors with fill-pointers.

(defvar %stacks%)

(defmacro with-stacks (names &body body)
  (multiple-value-bind (docs forms) (extract-doc-and-declares body)
    `(let ,(mapcar #'(lambda (name)
		       `(,name (cond ((> (length %stacks%) 0)
				      (let ((res (vector-pop %stacks%)))
					(setf (fill-pointer res) 0)
					res))
				     (t (make-array 1000 :adjustable t
						    :fill-pointer 0)))))
		   names)
       ,@docs
       (unwind-protect
           (progn ,@forms)
	 ,@(mapcar #'(lambda (name)
		       `(progn
			  (setf (fill-pointer ,name) 0)
			  (vector-push-extend ,name %stacks%)))
		   names)))))

)

(defun set-stack-length (stack new-length)
  (let ((current-length (fill-pointer stack))
	(underlying-length (array-dimension stack 0)))
    (cond ((< new-length current-length)
	   ;; shrink the stack
	   (setf (fill-pointer stack) new-length))
	  ((< new-length underlying-length)
	   ;; Grow the stack - the underlying vector is big enough
	   (setf (fill-pointer stack) new-length)
	   (fill stack 'nil :start current-length :end new-length))
	  (t
	   ;; Grow the stack after extending the underlying vector
	   (adjust-array stack new-length)
	   (fill stack 'nil :start current-length :end new-length)
	   (setf (fill-pointer stack) new-length)))))


(defvar *changed-method-stacks*
  "A bunch of small vectors used to record which methods need recalculating
  for a flavor.")

(defun alloc-tiny-stack ()
  (if (eq 0 (fill-pointer *changed-method-stacks*))	;check to see if vector
      (make-array 10 :adjustable t :fill-pointer 0)	;over popped. moe 1/16/86
      (vector-pop *changed-method-stacks*)))

(eval-when (eval compile)

(defmacro dealloc-tiny-stack (place)
  `(let ((%x (shiftf ,place nil)))
     (when %x
       (setf (fill-pointer %x) (array-dimension %x 0))
       (fill %x nil)
       (setf (fill-pointer %x) 0)		;fixed by EB 11/19/85
       (vector-push-extend %x *changed-method-stacks*))))

)

;;; INIT-FLAVORS-STACKS initializes or reinitializes the stack data structures
;;; (so that they can be garbage collected away prior to disksave).

(defun init-flavors-stacks ()
  (setq %stacks%
	(make-array 10 :adjustable t :fill-pointer 0))
  (setq *changed-method-stacks*
	(make-array 10 :adjustable t :fill-pointer 0))
  t)

(init-flavors-stacks)
;;;
;;; More specific to Flavors.
;;; 

(eval-when (eval compile)

(defmacro flavor-function-name (symbol &rest things)
  "Usually called with flavor-name, method, and method-type.
  Interns the name in the package of the first thing (a symbol)."
  `(intern (concatenate
            'string (symbol-name ,symbol)
            ,@(mapcan #'(lambda (thing)
                          `("-"  (let ((thing ,thing))
                                   (if (symbolp thing)
                                       ;(prin1-to-string thing)
				       (symbol-name thing)
                                       thing))))
                      things))
           (if (eq (symbol-package ,symbol) (find-package "KEYWORD"))
		(find-package "FLAVORS")
		(symbol-package ,symbol))))

)

(defun set-name (iv)
  (cond ((get iv 'set-name))
        (t (let ((res (intern (concatenate 'string "SET-" (symbol-name iv))
                              (find-package 'keyword))))
             (setf (get iv 'set-name) res)
             res))))

(defun get-name (iv)
  (cond ((get iv 'get-name))
        (t (let ((res (intern (symbol-name iv) (find-package 'keyword))))
             (setf (get iv 'get-name) res)
             res))))

(eval-when (eval compile)

(defmacro combination-ordering (name)
  `(let ((%combo ,name))
     (or (get %combo 'ordering)
	 (error "No such combination: ~S." %combo))))

(defmacro combination-mixer (name)
  `(let ((%combo ,name))
     (or (get %combo 'mixer)
	 (error "No such combination: ~S." %combo))))

(defmacro make-combination (name ordering mixer)
  `(progn (setf (get ,name 'ordering) ,ordering
		(get ,name 'mixer) ,mixer)))

)
;;;
;;; Environments.  
;;;

;;; Special values for the default: REQUIRED and UNSUPPLIED.


(eval-when (eval compile)

(defstruct (method-env (:print-function private-structure-printer)
                       (:include iv-env))
  numordered ; number of vars ordered.
  defaults   ; default forms.
  (ables '#() :type simple-vector))

(defstruct (instance-env (:print-function private-structure-printer)
                         (:include method-env))
  required) ; list of required ivs.

(defbits ables
  gettable
  settable
  initable
  outside-accessible)

(defmacro var-able (x)
  `(aref ables
	 (or (position ,x var-stack)
	     (error "No such instance variable - ~S." ,x))))

)

(lucid::defstruct-runtime-slot-function instance-env vector instance-env)
