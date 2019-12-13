;;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LUCID; Base: 10 -*-
;;;;
;;;; /pubs/top/flavors/sun/work/flavors-functions.lisp, Module <module name>
;;;
;;; ***************************************************************************
;;;
;;;        Copyright (C) 1988 by Lucid, Inc.  All Rights Reserved
;;;
;;; ***************************************************************************
;;;
;;; Examples from 3.0 Flavors Guide, function pages
;;;
;;; Programmer: Kris Dinkel
;;;
;;; Edit-History:
;;;
;;; Created:  6-Jan-88 by kris
;;;
;;; End-of-Edit-History


;; p. 2, *all-flavor-names*

(defflavor new-flavor (x y z) () )

(not (null (member 'new-flavor *all-flavor-names*)))


;; p. 4, compile-flavor-methods

(defflavor flavor1 () () )           

(defflavor flavor2 () () )           

(make-instance 'flavor1)              ; Instantiate flavor1.

(compile-flavor-methods flavor2)      ; Force flavor2 to be compiled.

(make-instance 'flavor2)              ; Instantiate flavor2; there is


;; p. 6, continue-whopper, etc

;; The following example defines a whopper that returns nil if  
;; the first argument is 0 and otherwise causes the combined method
;; to be executed normally.

(defflavor my-flavor () ())          

(defmethod (my-flavor :my-message) (x) x)

(defwhopper (my-flavor :my-message)(arg1 &rest args)
    (declare (ignore args))
    (if (eql arg1 0)
	nil
	(continue-whopper-all)))

(setq x (make-instance 'my-flavor))

(send x :my-message 0)

(send x :my-message 3)


;; The following example replaces the previously defined whopper with a
;; new whopper that takes a list of all its arguments and passes that
;; list as a single argument to the combined method.

(defwhopper (my-flavor :my-message)(&rest args)
    (continue-whopper args))

(send x :my-message 3 4 5)

(send x :my-message)


;; p. 11, defflavor

(defflavor new-flavor
    (a-var b-var)
    ()
    :settable-instance-variables)


;; p. 12, defmethod

(defflavor simple-flavor
    (a-var b-var)
    ()
    :settable-instance-variables)

(defmethod (simple-flavor :subtraction) ()
  (- a-var b-var))

(setq simple-instance
      (make-instance 'simple-flavor
      :a-var 20
      :b-var 7))

(send simple-instance :subtraction)

(undefmethod (simple-flavor :subtraction))


;; p. 14, defwhopper

;; The following examples define and use a whopper that reverses the
;; order of the first two arguments that are passed to the message.

(defflavor new-flavor () () )          

(setq x (make-instance 'new-flavor))

(defmethod (new-flavor :message) (&rest args) args)

(send x :message 1 2 3 4)

(defwhopper (new-flavor :message)(arg1 arg2 &rest args)
   (lexpr-continue-whopper arg2 arg1 args))

(send x :message 1 2 3 4)

;; p. 15, defwrapper

(defflavor new-flavor () () )          

(defmethod (new-flavor :message) (x) x)

(setq x (make-instance 'new-flavor))

(send x :message 4)

(send x :message :key)

(defwrapper (new-flavor :message)((arg) . body)
   `(when (numberp arg)
       (- (progn ,@ body))))

(send x :message 4)

(send x :message :key)


;; p. 16, flavor-allowed-init-keywords

(defflavor first-flavor
      (a-var b-var)
      ()
      :settable-instance-variables)

(defflavor second-flavor
      (c-var d-var)
      (first-flavor)
      :settable-instance-variables)

(defflavor third-flavor
      (e-var f-var)
      (second-flavor)
      :settable-instance-variables)

(flavor-allowed-init-keywords 'third-flavor)

(flavor-allows-init-keyword-p 'third-flavor :a-var)

(flavor-allows-init-keyword-p 'first-flavor :f-var)


;; p. 18 instancep

(defflavor test-flavor () ())

(setq test-instance
      (make-instance 'test-flavor))

(instancep test-instance)


;; p. 19 make-instance

(defflavor demo-flavor
      (a-var b-var)
      ()
      :settable-instance-variables)

(setq demo-instance
      (make-instance 'demo-flavor))

(describe demo-instance)

(setq new-demo-instance
      (make-instance 'demo-flavor
      :a-var 20
      :b-var 30))

(describe new-demo-instance)


;; p. 21  self

(defflavor rectangle-flavor
      (height width)
      ()
      :settable-instance-variables)

(defmethod (rectangle-flavor :area) ()
      (* height width))

(defflavor cube-flavor
      (depth)
      (rectangle-flavor)
      :settable-instance-variables)

(defmethod (cube-flavor :volume) ()
      (* depth (send self :area)))

(setq cube-instance
      (make-instance 'cube-flavor
      :depth 3
      :height 5
      :width 4))

(send cube-instance :volume)

;; p. 22, send

(defflavor example-flavor
      (a-var b-var)
      ()
      :settable-instance-variables)

(defmethod (example-flavor :division) ()
      (/ a-var b-var))

(setq example-instance
      (make-instance 'example-flavor
      :a-var 40
      :b-var 8))

(send example-instance :division)

(defmethod (example-flavor :multiply-division) ()
      (* 3 (send self :division)))

(send example-instance :multiply-division)


;; p. 23 symeval-in-instance

(defflavor circle-flavor
      ((x-center 20)(y-center 30) radius)
      ()
      :settable-instance-variables)

(setq circle-instance
      (make-instance 'circle-flavor))

(symeval-in-instance circle-instance 'x-center)

(symeval-in-instance circle-instance 'radius nil 'unknown-radius)

(set-in-instance circle-instance 'y-center
      (symeval-in-instance circle-instance 'x-center))

(symeval-in-instance circle-instance 'width t)


