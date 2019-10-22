;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LUCID; Base: 10 -*-
;;;
;;;; compile-load-flavors.lisp, Module FLAVORS
;;;
;;; ***************************************************************************
;;;
;;;        Copyright (C) 1989 by Lucid, Inc.  All Rights Reserved
;;;
;;; ***************************************************************************
;;;
;;;  Script to compile and load flvors
;;;

(in-package 'user)

;;; should be in your flavors directory
(defun compile-load-flavors ()
  (load (compile-file "flavors-macros"))
  (load (compile-file "flavors-package"))
  (load (compile-file  "kkernel"))
  (load (compile-file  "kernel"))
  (load (compile-file  "flavor-main-decls"))
  (load (compile-file  "flavor-main0"))
  (load (compile-file  "flavor-main1"))
  (load (compile-file  "flavor-main2"))
  (load (compile-file  "vanilla"))
  (load (compile-file "last-file")))

(defun load-flavors ()
  (load "flavors-package")
  (load "kkernel")
  (load "kernel")
  (load "flavor-main-decls")
  (load "flavor-main0")
  (load "flavor-main1")
  (load "flavor-main2")
  (load "vanilla")
  (load "last-file"))

