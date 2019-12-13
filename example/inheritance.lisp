;;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LUCID; Base: 10 -*-
;;;;
;;;; /pubs/top/flavors/sun/work/inheritance.lisp, Module <module name>
;;;
;;; ***************************************************************************
;;;
;;;        Copyright (C) 1988 by Lucid, Inc.  All Rights Reserved
;;;
;;; ***************************************************************************
;;;
;;; Examples from 3.0 Flavors Guide, chapter Inheritance and Combined Methods
;;;
;;; Programmer: Kris Dinkel
;;;
;;; Edit-History:
;;;
;;; Created:  6-Jan-88 by kris
;;; 15-Sep-88 kris: Changed x-floor to floor.
;;;
;;; End-of-Edit-History


;; p. 3 flavor inheritance

(defflavor field
      (field-length field-width field-area)
      ()
      :settable-instance-variables)

(defflavor farm
      (number-of-fields total-area)
      (field)
      :settable-instance-variables)

(setq oldmacdonald
       (make-instance 'farm
       :field-length 0 
       :field-width 0
       :field-area 0
       :number-of-fields 0
       :total-area 0))

(defmethod (field :calculate-field-area)()
      (setq field-area (* field-length field-width)))    

(defmethod (farm :calculate-total-area)()               
      (setq total-area (* number-of-fields field-area)))

(send oldmacdonald :set-field-length 15)                

(send oldmacdonald :set-field-width 5)

(describe oldmacdonald)

(send oldmacdonald :calculate-field-area)               

(describe oldmacdonald)

(send oldmacdonald :set-number-of-fields 6)

(send oldmacdonald :calculate-total-area)               

(describe oldmacdonald)

;; p. 7, displaying inherited instance variables

(describe oldmacdonald)


;; p. 8, creating a combined method

(defflavor floor
      (floor-length floor-width floor-area)
      ()
      :settable-instance-variables)

(defflavor factory
      (number-of-floors total-area)
      (floor)
      :settable-instance-variables)

(defmethod (floor :area)()
      (send self :set-floor-area (* floor-length floor-width)))

(defmethod (factory :area)()
      (send self :set-total-area (* number-of-floors floor-area)))

(setq general-motors
	(make-instance 'factory
	:floor-length 15
	:floor-width 5
	:floor-area 0
	:number-of-floors 7
	:total-area 0))

(describe general-motors)

(send general-motors :area)


;; p. 11, using a simple daemon method combination

(undefmethod (floor :area))

(defmethod (floor :before :area)()
      (setq floor-area (* floor-length floor-width)))

(describe general-motors)

(send general-motors :area)

(describe general-motors)

;; p. 14, using a complex daemon method combination

(defflavor field                                      ; Define flavor
      ((field-length 4)(field-width 2)(field-area 0)) ; field.
      ()
      :settable-instance-variables)

(defmethod (field :before :area)()                    ; Define method to
      (setq field-area (* field-length field-width))) ; calculate field-

(defflavor keep-count                             
      ((count 0))                                     ; Define flavor 
      ()                                              ; keep-count.
      :settable-instance-variables)

(defmethod (keep-count :before :area)()               ; Define method to
      (setq count (+ count 1)))                       ; keep count.

(defflavor farm                                  
      ((total-area 0)(number-of-spaces 10))           ; Define flavor
      (field keep-count)                              ; farm.
      :settable-instance-variables)

(defmethod (farm :primary :area)()                    ; Define method to
      (setq total-area (* number-of-spaces field-area))) ; calculate

(defflavor county      
      ()                                              ; Define flavor
      (farm)                                          ; county, which
      :settable-instance-variables)                   ; inherits all

(defmethod (county :after :area)()                  
      (progn                                          ; Define method to
      (setq total-area 0)                             ; re-initialize
      (setq field-area 0)))                           ; instance vari-

(setq wiltshire
      (make-instance 'county))

(describe wiltshire)

(send wiltshire :area)

(describe wiltshire)

;; p. 19, using nondaemon method combination types

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
      (:method-combination (:progn :base-flavor-first :addup)))

(defmethod (first-flavor :addup)()
      (setq a-var (+ a-var b-var)))

(defmethod (second-flavor :addup)()
      (setq c-var (+ c-var d-var)))

(defmethod (third-flavor :addup)()
      (setq e-var (+ e-var f-var)))

(setq first-instance (make-instance 'first-flavor))

(describe first-instance)

(send first-instance :addup)

(describe first-instance)

(defflavor first-flavor
      ((a-var 1)(b-var 2))
      (second-flavor third-flavor)
      :settable-instance-variables
      (:method-combination (:and :base-flavor-last :addup)))

(setq second-instance (make-instance 'first-flavor))

(describe second-instance)

(send second-instance :addup)

(describe second-instance)

(defflavor first-flavor
      ((a-var 1)(b-var 2))
      (second-flavor third-flavor)
      :settable-instance-variables
      (:method-combination (:or :base-flavor-last :addup)))

(setq third-instance (make-instance 'first-flavor))

(send third-instance :addup)

(describe third-instance)

(defflavor first-flavor
      ((a-var 1)(b-var 2))
      (second-flavor third-flavor)
      :settable-instance-variables
      (:method-combination (:list :base-flavor-last :addup)))

(setq fourth-instance (make-instance 'first-flavor))

(send fourth-instance :addup)

;; p. 23

(defmethod (first-flavor :calculate)()
      (list (* a-var b-var)))

(defmethod (second-flavor :calculate)()
      (list (* c-var d-var)))

(defmethod (third-flavor :calculate)()
      (list (* e-var f-var)))

(defflavor first-flavor
      ((a-var 1)(b-var 2))
      (second-flavor third-flavor)
      :settable-instance-variables
      (:method-combination (:append :base-flavor-last :calculate)))

(setq fifth-instance (make-instance 'first-flavor))

(send fifth-instance :calculate)

(defflavor first-flavor
      ((a-var 1)(b-var 2))
      (second-flavor third-flavor)
      :settable-instance-variables
      (:method-combination (:nconc :base-flavor-first :calculate)))

(setq sixth-instance (make-instance 'first-flavor))

(send sixth-instance :calculate)


;; p. 26 (using daemon and nondaemon method types)

(defflavor field                                      ; Define flavor 
      ((field-length 4)(field-width 2)(field-area 0)) ; field.
      ()
      :settable-instance-variables)

(defmethod (field :before :area)()                    ; Define method to
      (setq field-area (* field-length field-width))) ; calculate field-

(defflavor keep-count                             
      ((count 0))                                     ; Define flavor 
      ()                                              ; keep-count.
      :settable-instance-variables)

(defmethod (keep-count :before :area)()               ; Define method to
      (setq count (+ count 1)))                       ; keep count.

(defflavor farm                                  
      ((total-area 0)(number-of-spaces 10))           ; Define flavor
      (field keep-count)                              ; farm.
      :settable-instance-variables)

(defmethod (farm :primary :area)()                    ; Define method to
      (setq total-area (* number-of-spaces field-area))) ; calculate

(defflavor factory                              
      ((total-area 0) (number-of-spaces 10))          ; Define flavor
      (floor keep-count)                              ; factory.
      :settable-instance-variables
      (:method-combination (:list :base-flavor-first :area)))

(defmethod (factory :primary :area) ()                ; Define method to
      (setq total-area (* number-of-spaces floor-area))) ;calculate

(defflavor floor                                      
      ((floor-length 5) (floor-width 3) (floor-area 0)); Define flavor 
      ()                                              ; floor.
      :settable-instance-variables)

(defmethod (floor :before :area) ()                   ; Define method to
      (setq floor-area (* floor-length floor-width))) ; calculate floor-

(defflavor county      
      ()                                              ; Define flavor
      (farm factory)                                  ; county, which
      :settable-instance-variables)                   ; inherits all

(defmethod (county :after :area) ()
      (progn                                          ; Define method to
      (setq total-area 0)                             ; re-initialize
      (setq field-area 0)))                           ; instance vari-

(setq wiltshire (make-instance 'county))

(describe wiltshire)

(send wiltshire :area)

(describe wiltshire)
