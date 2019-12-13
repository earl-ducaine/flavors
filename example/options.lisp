;;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LUCID; Base: 10 -*-
;;;;
;;;; /pubs/top/flavors/sun/work/options.lisp, Module <module name>
;;;
;;; ***************************************************************************
;;;
;;;        Copyright (C) 1988 by Lucid, Inc.  All Rights Reserved
;;;
;;; ***************************************************************************
;;;
;;; Examples from 3.0 Flavors guide, chapter 4, Keyword Options and the
;;;   Vanilla Flavor
;;;
;;; Programmer: Kris Dinkel
;;;
;;; Edit-History:
;;;
;;; Created:  6-Jan-88 by kris
;;; 14-Sep-88 kris: Replaced some obsolete examples with the right ones for 
;;;                   3.0 (examples on pp. 8-10); noted which examples go into
;;;                   the Debugger.
;;;
;;; End-of-Edit-History


;; p. 4 (using the keyword options)

(defflavor new-flavor
      (x-var y-var)
      ()
      (:init-keywords :x-var :y-var))

(setq new-instance
      (make-instance 'new-flavor
      :x-var 14
      :y-var 17))

(describe new-instance)

(defmethod (new-flavor :init)(x)
      (setq x-var (* (setq y-var 10) 3)))

(setq second-new-instance
      (make-instance 'new-flavor))

(describe second-new-instance)

(defflavor example-flavor
      (w-var x-var y-var z-var)
      ()
      :settable-instance-variables
      (:default-init-plist :w-var 1 :x-var 2 :y-var 3 :z-var 4))

(setq example-instance
      (make-instance 'example-flavor            
      :w-var 13
      :x-var 17))

(describe example-instance)

(defflavor alpha-flavor
      (t-var u-var v-var)                               
      ()
      :settable-instance-variables
      (:required-instance-variables w-var))          

(defflavor beta-flavor
      (x-var y-var z-var)                               
      (alpha-flavor)                            
      :settable-instance-variables)         

(setq beta-instance
      (make-instance 'beta-flavor
      :x-var 3                                  
      :y-var 5                                            
      :z-var 6))

(defflavor gamma-flavor
      (w-var x-var y-var z-var)                              
      (alpha-flavor)                             
      :settable-instance-variables)          

(setq gamma-instance
      (make-instance 'gamma-flavor               
      :w-var 4
      :x-var 3))

(describe gamma-instance)

;; p. 8

(defflavor demo-flavor
      (x-var y-var z-var)
      ()
      :settable-instance-variables
      (:required-init-keywords :y-var))

;; This invocation goes into the Debugger.
(setq demo-instance (make-instance 'demo-flavor
      :x-var 2
      :z-var 3))

(setq new-demo-instance (make-instance 'demo-flavor      
      :x-var 2
      :z-var 3
      :y-var 4))

(defflavor first-flavor () () (:required-methods :message))

(defflavor second-flavor () (first-flavor))

;; This invocation goes into the Debugger.
(setq second-flavor-in (make-instance 'second-flavor))

(defflavor third-flavor () ())

(defmethod (third-flavor :message) () (+ 1 1))

(defflavor fourth-flavor () (first-flavor third-flavor))

(setq fourth-flavor-in (make-instance 'fourth-flavor))

(defflavor a-ok () ())

(defflavor b-movie () ())

(defflavor c-grade () (a-ok b-movie) (:required-flavors d-pass))

(defflavor d-pass () ())

;; This invocation enters the Debugger.
(setq c-grade-in (make-instance 'c-grade))  

(defflavor effort () (c-grade))

;; This invocation enters the Debugger.
(setq effort-in (make-instance 'effort))    

(defflavor floozie () (c-grade d-pass))

(setq floozie-in (make-instance 'floozie))  

;; p. 11

(defflavor alpha-flavor                              
  (s-var t-var)
  ()
  :settable-instance-variables)

(defflavor beta-flavor                              
  (u-var v-var)
  ()
  :settable-instance-variables)

(defflavor gamma-flavor                              
  (w-var x-var)
  ()
  :settable-instance-variables)

(defflavor delta-flavor                              
  (y-var z-var)
  (alpha-flavor beta-flavor)
  :settable-instance-variables
  (:included-flavors gamma-flavor))

(setq delta-instance (make-instance 'delta-flavor))

(describe delta-instance)

;; p. 16 (system-defined messages)

(defmethod (circle-flavor :print-self)(stream prindepth)
   (declare (ignore prindepth))
   (format stream "#<circle of radius ~A, center (~A,~A)>" 
	 (send self :radius)
	 (send self :x-center)
	 (send self :y-center)))

(make-instance 'circle-flavor :radius 5 :x-center 3 :y-center 7)

(defflavor test-flavor
      (a-var b-var)
      ()
      :settable-instance-variables)

(setq test-instance
      (make-instance 'test-flavor
      :a-var 15
      :b-var 17))

(defmethod (test-flavor :describe)()
      (format *standard-output* 
      "This is an instance of TEST-FLAVOR.~%~
      Its instance variables are as follows:~%~
      A-VAR - current value ~A~%~
      B-VAR - current value ~A"
      (send self :a-var)
      (send self :b-var)))

(describe test-instance)

(defflavor specimen-flavor
      (a-var b-var)
      ()
      :settable-instance-variables)

(defmethod (specimen-flavor :first-message)(a b)
      (list a b))

(setq specimen-instance
      (make-instance 'specimen-flavor))

(send specimen-instance :which-operations)

(defmethod (vanilla-flavor :operation-handled-p)(message)
   (not (null (member message (send self :which-operations)))))

(send specimen-instance :operation-handled-p :first-message)

(defmethod (vanilla-flavor :send-if-handles)(message &rest args)
    (when (send self :operation-handled-p message)
       (apply #'send self message args)))

(send specimen-instance :send-if-handles :first-message 30 21)

(defflavor example-flavor ()())

(defmethod (example-flavor :unclaimed-message)(message &rest args)
      (declare (ignore args))
      (format *error-output*
      "The message ~A has not been defined for this flavor." message)
      nil)

;; This goes into the Debugger?
(send (make-instance 'm-flavor) :nonexistent)



;;; these will be added by peter later


 (defflavor a () () (:required-methods :message))

 (defflavor b () (a))

 (setq b-in (make-instance 'b))

  (defflavor c () ())

  (defmethod (c :message) () (+ 1 1))

  (defflavor d () (a c))

  (setq d-in (make-instance 'd))



  (defflavor a () ())

  (defflavor b () ())

  (defflavor c () (a b) (:required-flavors d))

  (defflavor d () ())

  (setq c-in (make-instance 'c))  ;this will break

  (defflavor e () (c))

  (setq e-in (make-instance 'e)) ;this will also break

  (defflavor f () (c d))

  (setq f-in (make-instance 'f))  ;this will be ok



  (defflavor z-flavor (a-var b-var) ())

  (defmethod (z-flavor :init) (instance-variables)
    (if (> a-var b-var) (setq b-var (* 2 a-var))))

  (setq z-instance (make-instance 'z-flavor
                                  :a-var 5
                                  :b-var 3))

  (describe z-instance)
    


