;;; -*- Mode: LISP; Syntax: Common-lisp; Package: FLAVORS -*-
;;; 
;;; *****************************************************************
;;;
;;; (c) Copyright 1986 by Lucid Inc., All Rights Reserved
;;;
;;; *****************************************************************
;;;
;;; Edit-History: test file for required-init-keywords
;;;
;;; Created: 12/29/85
;;; Reviewed: <Review-Date>
;;;
;;; 27-Jan-86 moe: removed (in-packge 'flavors) ,use (use-package 'flavors)
;;; 28-Jan-86 moe: don't try to import lucid:test nor  etest
;;; 19-Mar-87 susan: modified so wouldn't come into application environment
;;;                  due to "on purpose" errors during compilation
;;;  8-Apr-87 tomh: fixed broken eval stuff 
;;; 18-Nov-87 lnz: Removed format call.
;;;
;;; End-of-Edit-History
 
(in-package 'flavors)

(defvar *test-instance*)

;;;make sure we are starting with previously undefined flavors

(eval-when (eval compile load)

(setf (symbol-plist 'mixin) '())

(setf (symbol-plist 'key-test) '())

(setf (symbol-plist 'simple-key-test) '())

)

(defflavor simple-key-test (iv1 iv2) ()
  (:initable-instance-variables iv1)
  (:init-keywords :iv2)
  (:required-init-keywords :iv1 :iv2))


(defflavor mixin (miv1 miv2) ()
  :initable-instance-variables
  (:init-keywords :mixin-init-key)
  (:required-init-keywords :miv1 :mixin-init-key)
  )

(defflavor key-test (iv1 iv2) (mixin)
  :initable-instance-variables
  (:init-keywords :init-key)
  (:required-init-keywords :iv1 :init-key)
  )

(etest (make-instance 'simple-key-test )
       "make-instance omitting a required key")
(etest (make-instance 'simple-key-test :iv1 0)
       "make-instance omitting a required key")
(test (make-instance 'simple-key-test :iv1 0 :iv2 0)
      "make-instance including all required keys")
(etest (make-instance 'key-test)
       "make-instance omitting a required key")
(etest (make-instance 'key-test :iv1 0 :init-key 0)
       "make-instance omitting a key required by mixin")
(etest (make-instance 'key-test :miv1 0 :mixin-init-key 0)
       "make-instance omitting a key requried by base flavor")
(test (make-instance 'key-test :miv1 0 :mixin-init-key 0
		     :iv1 0 :init-key 0)
      "make-instance with all required keys")
