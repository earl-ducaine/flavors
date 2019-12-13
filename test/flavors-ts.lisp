;;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LUCID; Base: 10 -*-
;;;;
;;;; flavors-ts, MOULE: TEST
;;;
;;; ***************************************************************************
;;;
;;;        Copyright (C) 1987 by Lucid Inc.,  All Rights Reserved
;;;
;;; ***************************************************************************
;;;
;;; This file is the startup for flavors tests
;;;
;;; Author:	Tom Hempel
;;;
;;; Edit-History:
;;;
;;; Created: 11-may-87
;;;
;;; End-of-Edit-History

(in-package sys:*testing-package-name*)

(unless (find-package "FLAVORS")
        (format *trace-output* "~%flavors not present!~%")        (throw 'punt-this-test nil))


(defvar *hacked-defmethod* nil)

(eval-when (eval compile load)
(when (and (find-package "FLAVORS") (not (fboundp 'defmethod)))
  (setq *hacked-defmethod* t)
  (shadowing-import (mapcar #'(lambda(x)(find-symbol x "FLAVORS"))
			    '("DEFMETHOD" "UNDEFMETHOD" "MAKE-INSTANCE")))
  (use-package 'flavors)
  ))

(deftestfun string-embedded-p (string big-string)
  (search string big-string))

(deftestmacro test-stream-output (correct-string
				  form
				  &key
				  (print-stream '*error-output*)
 				  (test 'string-embedded-p))
                                                ;in case other warnings
						;get spewed out
  `(let* ((,print-stream (make-string-output-stream)))
     (get-output-stream-string ,print-stream)	;clear the error-stream
     ,form
     (,test ,correct-string
	      (get-output-stream-string ,print-stream))))

           
