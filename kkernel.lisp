;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: (FLAVOR-INTERNALS LISP) -*-
;;;
;;; Copyright (C) 1985 by Lucid Inc.,  All Rights Reserved
;;;
;;; The kernel-kernel allows object-oriented systems to share hooks.


(in-package :flavors)

(defun send (instance message &rest args)
  (apply 'flavor-send instance message args))

(define-compiler-macro send (instance message &rest args)
  `(flavor-send ,instance ,message ,@args))

;; (lucid::defstruct-simple-predicate %instance instancep)

(defparameter lucid::*flavors-instance-type* '%instance)

(defparameter lucid::*flavors-send* #'send)

(eval-when (:execute :compile-toplevel)

(defmacro alloc-instance (size id &optional (initial-element ''unbound))
  "Allocates a new instance."
  `(let ((array (lucid::new-structure (1+ ,size) '%instance)));moe 1/11/86
     (setf (lucid::structure-ref array 0 '%instance) ,id)
     (dotimes (i ,size)
       (setf (lucid::structure-ref array (1+ i) '%instance) ,initial-element))
      array)))

(defun slot-value (instance slot &optional type)
  (if type
      (error "slot-value not yet supported for flavors (i know)")
      (cl:slot-value instance slot)))

(defmacro %instance-ref (instance slot)
  `(slot-value ,instance ,slot '%instance))

(defmacro instance-descriptor (instance)
  ;; moe 1/11/86
  `(%instance-ref ,instance 0))

(defmacro slot-unbound-p (instance slot)
;  "Follows 'forwarding pointers'."       translate->add one to slot
  `(let ((thing (%instance-ref ,instance (1+ ,slot))))	;moe 1/11/86
     (eq 'unbound thing)))
