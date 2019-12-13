;;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LUCID; Base: 10 -*-
;;;;
;;;; /pubs/top/flavors/sun/work/whoppers.lisp, Module <module name>
;;;
;;; ***************************************************************************
;;;
;;;        Copyright (C) 1988 by Lucid, Inc.  All Rights Reserved
;;;
;;; ***************************************************************************
;;;
;;; Examples from 3.0 Flavors Guide, chapter 6, Whoppers and Wrappers
;;;
;;; Programmer: Kris Dinkel
;;;
;;; Edit-History:
;;;
;;; Created:  6-Jan-88 by kris
;;;
;;; End-of-Edit-History

;; p. 6 (using a whopper and a wrapper)

(defflavor test-flavor 
      (p-var q-var)
      ()
      :settable-instance-variables)

(defmethod (test-flavor :calculate)(x y)                        
      (setq p-var (+ (* p-var x)(* q-var y))))

(setq test-instance
      (make-instance 'test-flavor
      :p-var 2
      :q-var 3))

(send test-instance :calculate 4 5)

(describe test-instance)                                        

(defwhopper (test-flavor :calculate)(x y)
  (if (or (> x 10)(< y 5))
      (continue-whopper 3 6)
      (continue-whopper-all)))

(trace test-flavor-calculate-primary)

(send test-instance :calculate 11 9)

(defwrapper (test-flavor :calculate)((x y) . body)
`(if (< p-var 200) 
     ,@body 
     (progn (send self :set-p-var 0) 
	    ,@body)))

;; p. 7

(describe test-instance)

(send test-instance :calculate 9 7)

(send test-instance :calculate 2 5)

(defwrapper (test-flavor :calculate)((x y) . body)                    
`(if (> p-var 200)
     (progn (send self :set-p-var 0)
	    (if (> ,@body 75)                                  
		(send self :p-var)
		(progn (send self :set-p-var 0)
		       (format *standard-output* 
			     "The value is too small.")))) 
     (if (> ,@body 75)
	 (send self :p-var) 
	 (progn (send self :set-p-var 0)
		(format *standard-output*
		      "The value is too small.")))))

(describe test-instance)

(send test-instance :calculate 10 18)

(send test-instance :calculate 17 15)

(describe test-instance)

(undefmethod (test-flavor :wrapper :calculate))

(undefmethod (test-flavor :whopper :calculate))

;; p. 10 (Using whoppers with returned values)

(defflavor example-flavor
      (p-var q-var)
      ()
      :settable-instance-variables)

(defmethod (example-flavor :message)(x)
      (setq p-var (* q-var x)))

(defwhopper (example-flavor :message)(x)
      (if (> (continue-whopper-all) 20)
      (send self :set-p-var (/ (continue-whopper-all) 2))))

(setq example-instance                                            
      (make-instance 'example-flavor
      :p-var 5
      :q-var 12))

(trace example-flavor-message-primary)

(send example-instance :message 7)

(describe example-instance)

;; p. 12 (using multiple whoppers etc)

(defflavor second-flavor
      ((c-var 3)(d-var 4))
      ()
      :settable-instance-variables)

(defflavor third-flavor
      ((e-var 5)(f-var 6))
      ()
      :settable-instance-variables)

(defflavor first-flavor
      ((a-var 1)(b-var 2))
      (second-flavor third-flavor)
      :settable-instance-variables
      (:method-combination (:list :base-flavor-first :calculate)))

(defmethod (first-flavor :calculate)()
      (setq a-var (+ a-var b-var)))

(defmethod (second-flavor :calculate)()
      (setq c-var (+ c-var d-var)))

(defmethod (third-flavor :calculate)()
      (setq e-var (+ e-var f-var)))

(setq x 0)

(defwhopper (first-flavor :calculate)()
      (if (eql x 0)(progn (setq x (+ x 1))(continue-whopper-all))))

(defwhopper (second-flavor :calculate)()
      (if (eql x 1)(progn (setq x (+ x 5))(continue-whopper-all))))

(defwhopper (third-flavor :calculate)()
      (if (eql x 6)(progn (setq x (+ x 10))(continue-whopper-all))))

(setq first-instance
      (make-instance 'first-flavor))

(send first-instance :calculate)

(eql x 16)

(setq y 0)

(defwrapper (first-flavor :calculate)(() . body)
      `(if (eql y 0)(progn (setq y (+ y 1)) ,@body)))

(defwrapper (second-flavor :calculate)(() . body)
      `(if (eql y 1)(progn (setq y (+ y 5)) ,@body)))

(defwrapper (third-flavor :calculate)(() . body)
      `(if (eql y 6)(progn (setq y (+ y 10)) ,@body)))

(setq x 0)

(send first-instance :calculate)

(eql y 16)

(eql x 16)

(defwhopper (test-flavor :calculate) (x y)
  (if (> a-var 200)
   (progn (send self  :set-a-var 0)
    (if (> (continue-whopper-all) 75)
	 (send self :a-var) 
	 (progn (send self :set-a-var 0)
		(format *standard-output* "The value is too small."))))
   (if (> (continue-whopper-all) 75)
    (send self :a-var)
    (progn (send self :set-a-var 0)
      (format *standard-output* "The value is too small.")))))

