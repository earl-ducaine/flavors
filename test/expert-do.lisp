;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: LUCID; -*-
;;;;
;;;; expert-do,  Module TEST
;;;
;;; **************************************************************************
;;;
;;;       (c) Copyright 1987, 1988 by Lucid Inc.,  All Rights Reserved
;;;
;;; **************************************************************************
;;;
;;; This file runs the expert sysystem simulator
;;;  [The Common Lisp tests were "snapshot'ed" from Lucid's internal
;;;   test and development suite during mid-September 1986]
;;; Programmer: tom hempel 
;;;
;;; Edit-History: 
;;;
;;; Created  25-feb-87 tomh 
;;;  3-Nov-88 hardy: changed Module to TEST from CLTEST, updated copyright
;;;                  notice, cleaned up header
;;;
;;; End-of-Edit-History

(progn (gc)
       (time (demo nil)))
