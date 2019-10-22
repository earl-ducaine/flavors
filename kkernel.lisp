;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: (FLAVOR-INTERNALS LISP) -*-
;;; ***************************************************************************
;;;
;;;        Copyright (C) 1985 by Lucid Inc.,  All Rights Reserved
;;;
;;; ***************************************************************************
;;; KKERNEL.LISP
;;; The kernel-kernel allows object-oriented systems to share hooks.
;;;

(in-package "FLAVORS")


(defun send (instance message &rest args)
  (apply 'flavor-send instance message args))

(lucid::declare-machine-class lucid::common)

(lucid::def-compiler-macro send (instance message &rest args)
  `(flavor-send ,instance ,message ,@args))

(lucid::undeclare-machine-class)

(lucid::defstruct-simple-predicate %instance instancep)

(defparameter lucid::*flavors-instance-type* '%instance)

(defparameter lucid::*flavors-send* #'send)

(eval-when (eval compile)

(defmacro alloc-instance (size id &optional (initial-element ''unbound))
  "Allocates a new instance."
  `(let ((array (lucid::new-structure (1+ ,size) '%instance)));moe 1/11/86
     (setf (lucid::structure-ref array 0 '%instance) ,id)
     (dotimes (i ,size)
       (setf (lucid::structure-ref array (1+ i) '%instance) ,initial-element))
      array))

)

(defmacro %instance-ref (instance slot)
  `(lucid::structure-ref ,instance ,slot '%instance))	;moe 1/11/86

(defmacro instance-descriptor (instance)
  `(%instance-ref ,instance 0))			;moe 1/11/86
  
(defmacro slot-unbound-p (instance slot)
;  "Follows 'forwarding pointers'."       translate->add one to slot
  `(let ((thing (%instance-ref ,instance (1+ ,slot))))	;moe 1/11/86
     (eq 'unbound thing)))
