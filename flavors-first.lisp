;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LUCID; Base: 10 -*-
;;;
;;;; flavors-first.lisp, Module FLAVORS
;;;
;;; ***************************************************************************
;;;
;;;        Copyright (C) 1990 by Lucid, Inc.  All Rights Reserved
;;;
;;; ***************************************************************************
;;;
;;; The first file of the FLAVORS module.
;;;
;;;
;;; Edit-History:
;;;
;;; Created:  2-May-90 by meekie
;;;
;;;
;;; End-of-Edit-History


(in-package "LUCID")

(when (memq :FLAVORS *features*)
  (warn "The FLAVORS module is already loaded.")) 
