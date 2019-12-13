;;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LUCID; Base: 10 -*-
;;;;
;;;; /pubs/top/flavors/sun/work/flavors.lisp, Module <module name>
;;;
;;; ***************************************************************************
;;;
;;;        Copyright (C) 1988 by Lucid, Inc.  All Rights Reserved
;;;
;;; ***************************************************************************
;;;
;;; Examples from 3.0 Flavors Guide, chapter 2, Using the Flavor System
;;;
;;; Programmer: Kris Dinkel
;;;
;;; Edit-History:
;;;
;;; Created:  6-Jan-88 by kris
;;; 14-Sep-88 kris: Noted which examples enter the Debugger.
;;;
;;; End-of-Edit-History

;; p. 4 defining a flavor

(defflavor circle-flavor
    (x-center y-center radius)
    ()
    :initable-instance-variables
    :settable-instance-variables
    :gettable-instance-variables)


;; p. 6, methods for circle-flavor

(defmethod (circle-flavor :diameter) ()
    (* 2 radius))

(defmethod (circle-flavor :area) ()
    (* pi radius radius))             ; pi is a built-in constant

(defmethod (circle-flavor :moveright) (amount)
    (setq x-center (+ x-center amount)))


;; p. 6, instance of circle-flavor

(setq circle-instance
       (make-instance 'circle-flavor
		      :x-center 3
		      :y-center 7
		      :radius 10))


;; p. 7

(describe circle-instance)


;; p. 8

(send circle-instance :radius)

(send circle-instance :area)

(send circle-instance :moveright 15)

(send circle-instance :x-center)

(send circle-instance :set-x-center 9)

(send circle-instance :x-center)


;; p. 8, specifying default values

(setq x 2)

(defflavor test-flavor
      ((p-var 1) (q-var (* x 3)))
      ()
      :settable-instance-variables)

(setq first-instance
      (make-instance 'test-flavor))

(describe first-instance)


;; p. 9, defining flavors with no instance variables

(defflavor art ()()) 

(setq music (make-instance 'art))

(defmethod (art :composers) (a b) (list a b))

(send music :composers 'handel 'bach)


;; p. 10, specifying method arguments

(defflavor first-flavor ()())	              	

(setq first-instance (make-instance 'first-flavor))

(defmethod (first-flavor :message) (w x &optional y &rest z)
    (list w x y z))

(send first-instance :message 1 2)

(send first-instance :message 1 2 3 4)

(defflavor second-flavor ()())

(setq second-instance (make-instance 'second-flavor))

(defmethod (second-flavor :new-message) (w &optional x &key key1 key2)
      (list w x key1 key2))

(send second-instance :new-message 3 5 :key2 'abc :key1 'xyz)


;; p. 12, defining a new method

(defmethod (circle-flavor :circumference) ()
  (* pi (send self :diameter)))

(send circle-instance :circumference)


;; p. 12, redefing existing methods

(send circle-instance :x-center)

(send circle-instance :moveright 10)

(defmethod (circle-flavor :moveright) (amount)
  (setq x-center (* x-center amount)))

(send circle-instance :moveright 10)

;; p. 13, deleting methods

(undefmethod (circle-flavor :moveright))

;; This invocation enters the Debugger.
(send circle-instance :moveright 1)

;; p. 14. independence of instance variables

(defflavor new-circle-flavor                       
      (x-center y-center radius)
      ()
      :settable-instance-variables)

(setq new-circle-instance
      (make-instance 'new-circle-flavor
      :x-center 3
      :y-center 4
      :radius 5))

(setq second-circle-instance                      
      (make-instance 'circle-flavor
      :x-center 20
      :y-center 20
      :radius 2))

(describe new-circle-instance)

(describe circle-instance)

(describe second-circle-instance)


;; p. 16 independent and shared messages

;; This invocation goes into the Debugger.
(send new-circle-instance :area)

(defflavor rectangle-flavor                 ; Define a flavor named
      (height width)                        ; rectangle-flavor.
      ()
      :settable-instance-variables)

(defmethod (rectangle-flavor :area)()       ; Define a message named
      (* height width))                     ; :area whose method is

(setq rectangle-instance
      (make-instance 'rectangle-flavor      ; Make an instance of
      :height 25                            ; the flavor.
      :width 2))

(describe rectangle-instance)

(describe circle-instance)

(send circle-instance :area)                ; (* pi radius).

(send rectangle-instance :area)

;; p. 18 tracing flavors
 
(defflavor new-rectangle-flavor                 ; Define a flavor.
      (height width)
      ()
      :settable-instance-variables)

(defmethod (new-rectangle-flavor :area)()       ; Define a method.
      (* height width))

(trace new-rectangle-flavor-area-primary)       ; Call the macro trace.

(setq new-rectangle-instance                    ; Make an instance of
      (make-instance 'new-rectangle-flavor      ; the flavor.
      :height 30
      :width 10))

(send new-rectangle-instance :area)             ; Send a message.

(untrace new-rectangle-flavor-area-primary)     ; Restore the flavor 
                                                ; to its normal state.
