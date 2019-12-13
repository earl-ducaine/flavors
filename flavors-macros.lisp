;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LUCID; Base: 10 -*-
;;;
;;; flavors-macros.lisp, Module FLAVORS

;;; Copyright (C) 1989 by Lucid, Inc.  All Rights Reserved

;;; auxiliary macros for compiling flavors

(in-package :lucid)

;;; from setf-decls.lisp

;;; Table giving simple setf inverse (if any) for a symbol key
(defvar *setf-inverse-table*)

;; Table giving simple setf expander function (if any) for a symbol
;; key
(defvar *simple-setf-method-table*)

;; Table giving setf expander function (if any) for a symbol key
(defvar *setf-method-expander-table*)

(defmacro remove-setf-inverse (x)
  `(remhash ,x *setf-inverse-table*))

(defmacro remove-simple-setf-method (x)
  `(remhash ,x *simple-setf-method-table*))

(defmacro get-setf-method-expander (x)
  `(gethash ,x *setf-method-expander-table*))
