;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: USER; -*-
;;; 
;;;; flav-eg-te, Module TEST/FLAVORS
;;;
;;; **************************************************************************
;;;
;;;  (c) Copyright 1986, 1987, 1988, 1989 by Lucid Inc.,  All Rights Reserved
;;;
;;; **************************************************************************
;;;
;;; This file contains all the examples that appear in the flavors section
;;; of the documentation.  
;;;
;;; Edit-History:
;;;
;;;  6-Mar-86 fy: Created by FY: 
;;; 10-Mar-86 moe: changed tests with :unclaimed-message to call with
;;;                message name
;;; 20-Jul-86 lnz: Conditionalize magic constants for areas in
;;;		   "testing new messages" since only the APOLLO currently has
;;;		   a correct pi since it is compiled natively.
;;; 23-Jul-86 moe: remove calls to defflavor within other functions since
;;;                only top-level calls to defflavor are allowed.

;;;  2-Oct-86 meg: added users guide examples
;;; 17-Nov-86 lnz: Fixed bug in "testing new messages":
;;;		   (case *target-machine-name* (apollo sun ... val)) ==>
;;;		   (case *target-machine-name* ((apollo sun ...) val)) ==>
;;; 19-Nov-86 lnz: flavors-system ==> flavor-internals.
;;; 24-Nov-86 moe: add tes to case statement
;;; 25-Nov-86 meg: bug 1308
;;;  6-Mar-87 pw: added Bug test #1578
;;; 19-Mar-87 susan: made runnable in application lisp
;;; 31-Jul-87 meekie: moved tests for bug #1308 and #1578 to flav-bug-te and
;;;                   flav-bugc-te (so that it can run compiled), respectively.
;;; 21-Oct-87 moe: add examples for "Using Daemon methods..."
;;; 20-Nov-87 tomh: changes for 3.0 
;;; 17-May-88 pw: fix "unhandled message" test.
;;;  9-Oct-88 maj: add PA to a case statement
;;;  6-Nov-88 maj: add kill-sub-lisp
;;; 11-Apr-89 hardy: updated header, added title, added SUNRISE to 
;;;                  *target-machine-name* lists.
;;;
;;; End-of-Edit-History

;;; NOTE that these tests can't be run more than once in a given image;
;;;      (tests which didn't fail the 1st time through WILL fail the 2nd
;;;       time); fix it sometime.

(in-package sys:*testing-package-name*)

(defvar *tolerable-float* (* 5 short-float-epsilon))

(defun tolerable-value (x y)
    (< (abs (- x y)) *tolerable-float*))

(defflavor circle-flavor
    (x-center y-center radius)
    ()
    :initable-instance-variables
    :settable-instance-variables
    :gettable-instance-variables)

(defmethod (circle-flavor :diameter) ()
    (* 2 radius))
(defmethod (circle-flavor :area) ()
    (* pi radius radius))             ; pi is a built-in constant

(defmethod (circle-flavor :moveright) (amount)
    (setq x-center (+ x-center amount)))

(defmethod (circle-flavor :perimeter) ()
    (* pi (send self :diameter)))


;;; alternate definition of circle-flavor.  Make sure we get the
;;; same results, and that it can be read back in.

(defflavor circle-flavor2
    (x-center y-center diameter)
    ()
    :initable-instance-variables
    :settable-instance-variables
    :gettable-instance-variables)

(defmethod (circle-flavor2 :radius) ()
    (/ diameter 2))

(defmethod (circle-flavor2 :area) ()
    (* 1/4 pi diameter diameter))

(defmethod (circle-flavor2 :moveright) (amount)
    (setq x-center (+ x-center amount)))

(let* ((random1 (random 100))
       (random2 (random 100))
       (random3 (random 100))
       (circle1 (make-instance 'circle-flavor
		      :x-center random1 :y-center random2 :radius random3))
       (circle2 (make-instance 'circle-flavor2
		      :x-center random1 :y-center random2
		      :diameter (* 2 random3))))
  (test (eql (send circle1 :x-center) (send circle2 :x-center))
	"testing equality of circle-flavor, circle-flavor2")
  (test (eql (send circle1 :y-center) (send circle2 :y-center))
	"ditto")
  (test (eql (send circle1 :radius) (send circle2 :radius))
	"ditto")
  (test (eql (send circle1 :diameter)(send circle2 :diameter))
	"ditto")
  (test (tolerable-value (send circle1 :area)(send circle2 :area))
	"ditto"))


;;; definition of rectangle-flavor

(defflavor rectangle-flavor 
    (height width top-left-x top-left-y) () 
    :initable-instance-variables
    :settable-instance-variables
    :gettable-instance-variables)

    
(defmethod (rectangle-flavor :area)  ()
    (* height width))

(defflavor farmland-flavor
    (types-of-crops soil-acidity foo)
    (rectangle-flavor)
    :initable-instance-variables
    :settable-instance-variables
    :gettable-instance-variables)

    



(test (setq a-farm (make-instance 'farmland-flavor :height 5 :width 10))
    "make sure farmland-flavor is well defined")

(test (eql (send a-farm :area) 50)  "make sure area of farmland works")



(defvar *default-x-center* 0)
(defvar *default-y-center* 0)


(defflavor circle-flavor3
   ((x-center *default-x-center*)
    (y-center *default-y-center*)
     radius)
   (circle-flavor)
   :gettable-instance-variables
   :settable-instance-variables
   :initable-instance-variables)


(test (and (instancep (setq a-circle (make-instance 'circle-flavor3 
						    :y-center 3 :radius 5)))
	   (eql (send a-circle :radius) 5)
	   (eql (send a-circle :diameter) 10)
	   (eql (send a-circle :set-x-center (send a-circle :y-center)) 3)
	   (eql (send a-circle :x-center) 3)
	   (tolerable-value (send a-circle :area)
			    (case *target-machine-name*
			      ((vax-ultrix vax-vms apollo hp300 pa sun sun2 sun3
				sunrise ncr tes)
			       78.53981633974483)
			      (t 78.53981)))
	   (eql (send a-circle :set-radius 2) 2)
	   (tolerable-value (send a-circle :area) ; this one is problematical
			    (case *target-machine-name*
			      ((vax-ultrix vax-vms apollo hp300 pa sun sun2 sun3
				sunrise ncr tes)
			       12.566370614359172)
			      (t 12.56637))))
      "testing new messages")

(test (and (instancep (setq another-circle (make-instance 'circle-flavor3 
					       :x-center 3
					       :y-center 7
					       :radius 10)))
	   (eql (send another-circle :set-radius 3) 3)
	   (eql (send another-circle :diameter) 6))
      "make-instance  - users guide ex")


;; we're no in the mixing section

#|
(defflavor plist-mixin () ())

(defflavor farmland-with-plist-mixin
    ()                                 ; no new instance variables
    (farmland-flavor plist-mixin))    ; component flavors
|#

   (test (and (eq (defflavor plist-mixin 
		    ((property-list nil))
		    ()) 'PLIST-MIXIN)
	      (eq (defmethod (plist-mixin :putprop) (property value)
		    (setf (getf property-list property) value)) ':PUTPROP)
	      (eq (defmethod (plist-mixin :getprop) 
		    (property &optional default)
		    (getf property-list property default)) ':GETPROP)
	      (eq (defmethod (plist-mixin :remprop) (property)
		    (remf property-list property)) ':REMPROP))
	 "plist-mixin - users guide ex #1")

   (test (eq (defflavor farmland-with-plist-mixin
	       ()			; no new instance variables
	       (farmland-flavor plist-mixin)) ; component flavors
	     'FARMLAND-WITH-PLIST-MIXIN)
	 "plist-mixin - users guide ex #2")

   (test (make-instance 'farmland-with-plist-mixin)
	 "making sure that farmland-with-plist-mixin works")


;;; let's no make a dummy flavor so that we can test out defwhopper and
;;; defflavor

(defflavor a-flavor () ())

(defmethod (a-flavor :a-message) (&optional a throw-tag throw-value)
  (if throw-tag
      (throw throw-tag throw-value)
      (1+ a)))

;;; that was ugly, but useful.  Let's test out this defwhopper

(defwhopper (a-flavor :a-message) (&rest args)
   (declare (ignore args))
   (catch 'a-throw (continue-whopper-all)))

(let ((x (make-instance 'a-flavor)))
  (test (eql (send x :a-message 3) 4) "normal run of defwhopper")
  (test (eql (send x :a-message 3 'a-throw :foo) :foo)
	"catching the throw")
  (etest (send x :a-message 3 'new :foo)
	 "no one to catch"))


;;; now let's try the wrapper instead of the whopper.


(undefmethod (a-flavor :whopper :a-message))

(defwrapper (a-flavor :a-message) ((arg1 arg2). body)
   `(unless (null arg1)
	. ,body))


(let ((x (make-instance 'a-flavor)))
  (test (eql (send x :a-message nil) nil) "wrapper catches it")
  (test (eql (send x :a-message 3) 4) "wrapper lets it through"))

(undefmethod (a-flavor :wrapper :a-message))          

;;; now we move into vanilla flavor.  I hope that this works like I claim
;;; it will

   (test (eq
	  (defmethod (circle-flavor :print-self) (stream prindepth)
	    (declare (ignore prindepth print-escape-p))
	    (format stream "#<circle of radius ~A, center (~A,~A)>" 
		    (send self :radius)
		    (send self :x-center)
		    (send self :y-center)))
	  :print-self)
	 "defmethod returns message name")


   (test (eql
	  (defmethod (circle-flavor :unclaimed-message)(msg \&rest args)
	    (declare (ignore args))
	    (format *error-output* "I'm ignoring a message of type ~A~%" msg)
	    nil)
	  :UNCLAIMED-MESSAGE)  "ditto")

   (let* ((x (make-instance 'circle-flavor :radius 5 :x-center 3 :y-center 7))
	  out result)
     (setq out  (with-output-to-string (*standard-output*) (princ x)))
     (test (string= out "#<circle of radius 5, center (3,7)>")
	   ":print-self works")
     (setq out (with-output-to-string (*error-output*)
		 (test (null (send (make-instance 'circle-flavor)
				   :unclaimed-message
				   :youve-never-heard-of-this))
		       "unclaimed message works")))

     (test (string= out

		    "I'm ignoring a message of type YOUVE-NEVER-HEARD-OF-THIS
")  "second text of :unclaimed-message")
     (test x  "extra test of make-instance"))

;;; we are now in the summary section.

;;; this is *all-flavor-names*

(defflavor new-flavor (x y z) () )
(test (eql (not (null (member 'new-flavor *all-flavor-names*)))  t)
      "verifying output of *all-flavor-names*")



;;; now we're checking compile-flavor-methods

  (defflavor flavor1 () () )
  (defflavor flavor2 () () )

  (test (and (eq (defmethod (flavor1 :msg1) 
		   (a b &optional c &rest d)
		   (list a b c d)) :msg1)
	     (instancep (setq x1 (make-instance 'flavor1)))
	     (equal (send x1 :msg1 1 2) '(1 2 NIL NIL))
	     (equal (send x1 :msg1 1 2 3 4) '(1 2 3 (4))))
	"sending mesgs - users guide ex#1")


  (test (and (eq (defmethod (flavor1 :msg2) 
		   (a &optional c &key key1 key2)
		   (list a c key1 key2)) :msg2)
	     (equal (send x1 :msg2 3 5 :key2 'abc :key1 'xyz) 
		    '(3 5 XYZ ABC))
	     (equal (send x1 :msg2 :key1 'abc :key2 'xyz) 
		    '(:KEY1 ABC NIL XYZ)))
	"sending mesgs - users guide ex#2")

  (test (instancep (make-instance 'flavor1)) "verifying an instance")
  (test (null (compile-flavor-methods flavor2))
	"compile-flavor-methods returns nil")
  (test (instancep (make-instance 'flavor2)) "compile-flavor-methods-n")

  (test (and (eq (defmethod (flavor2 :msg2) 
		   (a b c &optional d)
		   (list a b c d)) :msg2)
	     (instancep (setq x2 (make-instance 'flavor2)))
	     (equal (send x2 :msg2 :key1 'abc :key2 'xyz) 
		    '(:KEY1 ABC :KEY2 XYZ)))
	"sending mesgs - users guide ex #3")
;;; lets create a dummy flavor and methods, so we can verify the claims
;;; about defwhopper

  (defflavor my-flavor () ())
  (test (and (eq (defmethod (my-flavor :my-message) (x) x) :my-message)
	     (eq (defwhopper (my-flavor :my-message) (arg1 &rest args)
		   (declare (ignore args))
		   (if (eql arg1 0)
		       nil
		       (continue-whopper-all))) 
		 :my-message))
	"continue-whopper-1")


  (test (and (instancep (setq x (make-instance 'my-flavor)))
	     (null (send x :my-message 0) )
	     (= (send x :my-message 3) 3))
	"continue-whopper 2")
      
  (test (and (eq (defwhopper (my-flavor :my-message) (&rest args)
		   (continue-whopper args))
		 :my-message)
	     (equal (send x :my-message 3 4 5) '(3 4 5))
	     (null (send x :my-message)))
	"continue whopper 3")

  (undefmethod (my-flavor :my-message))
  (undefmethod (my-flavor :whopper :my-message))


;;; defflavor

  (defflavor circle-with-plist-flavor
    ((x-center 0)
     (y-center 0)
     radius)
    (plist-mixin)
    :settable-instance-variables
    (:required-methods :getprop :putprop))
  (setq pm (defflavor plist-mixin () ()))
  (test    (eq pm  'plist-mixin)
	   "defflavor 1")

  (defmethod (plist-mixin :getprop) () )
  (defmethod (plist-mixin :putprop) () )


  (test (and (instancep (setq x (make-instance 'circle-with-plist-flavor)))
	     (zerop (send x :x-center)))
	"defflavor 2")


;;; now we move into the defmethod section. Sure hope that it works

  (test  (and (eq (defmethod (circle-flavor :print-self)(stream prindepth)
		    (declare (ignore prindepth))
		    (format stream "#<circle of radius ~A>" radius))
		  :PRINT-SELF)
	      (instancep (setq x(make-instance 'circle-flavor :radius 3)))
	      (eq (undefmethod (circle-flavor :print-self))
		  :print-self))
	 "defmethod")

;;; now we're into defwhopper

  (defflavor new-flavor () () )
  (test (and (eq  (defmethod (new-flavor :msg) (&rest x) x)
		  :msg)
	     (instancep (setq x (make-instance 'new-flavor)))
	     (equal (send x :msg 1 2 3 4) '(1 2 3 4))
	     (equal (defwhopper (new-flavor :msg)(arg1 arg2 &rest args)
		      (lexpr-continue-whopper arg2 arg1 args))
		    :MSG)
	     (equal (send x :msg 1 2 3 4) '(2 1 3 4)))
	"test of defwhopper")

  (undefmethod (new-flavor :whopper :msg))

;;; now we're into the big time.  defwrapper

  (defflavor new-flavor () () )
  (test (and (eq (defmethod (new-flavor :message) (x) x)
		 :MESSAGE)
	     (instancep (setq x (make-instance 'new-flavor)))
	     (=  (send x :message 4) 4)
	     (eq (send x :message :key) :KEY)
	     (eq (defwrapper (new-flavor :message)((arg) . body)
		   `(when (numberp arg)
		      (- (progn ,@ body))))
		 :MESSAGE)
	     (= (send x :message 4) -4)
	     (null (send x :message :key)))
	"defwrapper")

  (without-cleaning-flavors
   (undefmethod (new-flavor :wrapper :message))
   (undefmethod (new-flavor :message))
   )


(defflavor new-flavor
      (var1 var2)
      (circle-flavor)
      :settable-instance-variables
      :gettable-instance-variables
      :initable-instance-variables
      (:init-keywords :key1 :key2))

(test (and (equal (flavor-allowed-init-keywords 'circle-flavor)
		  '(:RADIUS :X-CENTER :Y-CENTER))
	   (equal (flavor-allowed-init-keywords 'new-flavor)
		  '(:KEY1 :KEY2 :RADIUS :VAR1 :VAR2 :X-CENTER :Y-CENTER))
	   (eql (flavor-allows-init-keyword-p 'new-flavor :key1) 'NEW-FLAVOR)
	   (eql (flavor-allows-init-keyword-p 'new-flavor :no-such-key) nil))
      "flavor-allowed-init-keywords, etc.")



;;; now to instancep

(test (and (instancep (make-instance 'circle-flavor))
	     (null (instancep 7)))
	"instancep")



;;; now we're into self

  (defflavor rectangle-with-perimeter-flavor
    ()
    (rectangle-flavor)
    (:required-methods :height :width))
  (test (and (eq (defmethod (rectangle-with-perimeter-flavor :perimeter) ()
		   (* 2 (+ (send self :height)
			   (send self :width))))
		 :perimeter)
	     (instancep (setq x (make-instance 'rectangle-with-perimeter-flavor 
					       :width 7 :height 5)))
	     (= (send x :perimeter) 24))
	"self examples")

;;; and now to send

  (test (and (instancep 
	      (setq a-circle (make-instance 'circle-flavor :x-center 3)))
	     (eql (symeval-in-instance a-circle 'x-center) 3)
	     (eql  (symeval-in-instance a-circle 'radius nil 'unknown-radius)
		   'UNKNOWN-RADIUS)
	     (eql 
	      (set-in-instance 
	       a-circle 'y-center (symeval-in-instance a-circle 'x-center))
	      3)
	     (null (symeval-in-instance a-circle 'width t)))
	"symeval in instance")

  (test (and (eq (defflavor acircle-flavor 
		   (x-center y-center radius)
		   ()
		   :gettable-instance-variables) 'ACIRCLE-FLAVOR)
	     (instancep (setq a-circle1 
			      (make-instance 'acircle-flavor)))
	     (null (describe a-circle1)))
	"flavors - users guide ex #4")

  (test (eq (send a-circle1 :radius) 'FLAVOR-INTERNALS::UNBOUND)
	"simple instance - users guide ex")

  (if (not *sub-lisp-ok*)
      (warn "unhandled message test SKIPPED; *SUB-LISP-OK* <- NIL")
      (top-test "unhandled message test - users guide ex "
		(top (in-package "FLAVORS"))
		(top (defflavor acircle-flavor 
		       (x-center y-center radius)
		       ()
		       :gettable-instance-variables))
		(substr "ACIRCLE-FLAVOR")
		(top (setq a-circle1 (make-instance 'acircle-flavor)))
		(top (send a-circle1 :area) ->)
		(substr
	  ">>Error: Unhandled message :AREA in instance #<Instance ACIRCLE-FLAVOR"
	  )
		(top :a)))

(eval-when (eval compile load)
  (setf (symbol-plist 'field) nil)
  (setf (symbol-plist 'keep-count) nil)
  (setf (symbol-plist 'farm) nil)
  (setf (symbol-plist 'factory) nil)
  (setf (symbol-plist 'floor) nil)
  (setf (symbol-plist 'county) nil))

 (defflavor field                                      ; Define flavor 
        ((field-length 4)(field-width 2)(field-area 0)) ; field.
        ()
        :settable-instance-variables)
;FIELD
 (defmethod (field :before :area)()                    ; Define method to
        (setq field-area (* field-length field-width))) ; calculate field-
;:AREA                                                   ; area.
 (defflavor keep-count                             
        ((count 0))                                     ; Define flavor 
        ()                                              ; keep-count.
        :settable-instance-variables)
;KEEP-COUNT
 (defmethod (keep-count :before :area)()               ; Define method to
        (setq count (+ count 1)))                       ; keep count.
;:AREA
 (defflavor farm                                  
        ((total-area 0)(number-of-spaces 10))           ; Define flavor
        (field keep-count)                              ; farm.
        :settable-instance-variables)
;FARM
 (defmethod (farm :primary :area)()                    ; Define method to
        (setq total-area (* number-of-spaces field-area))) ; calculate
;:AREA                                                   ; total field-area.
 (defflavor floor                                      
        ((floor-length 5) (floor-width 3) (floor-area 0)); Define flavor 
        ()                                              ; floor.
        :settable-instance-variables)
;FLOOR
 (defmethod (floor :before :area) ()                   ; Define method to
        (setq floor-area (* floor-length floor-width))) ; calculate floor-
;:AREA                                                   ; area
 (defflavor factory                              
        ((total-area 0) (number-of-spaces 10))          ; Define flavor
        (floor keep-count)                              ; factory.
        :settable-instance-variables
        (:method-combination (:list :base-flavor-first :area)))
;FACTORY
 (defmethod (factory :primary :area) ()                ; Define method to
        (setq total-area (* number-of-spaces floor-area))) ;calculate
;:AREA                                                   ; total floor-area.
 (defflavor county      
        ()                                              ; Define flavor
        (farm factory)                                  ; county, which
        :settable-instance-variables)                   ; inherits all
;COUNTY                                                  ; other flavors.
 (defmethod (county :after :area) ()
        (progn                                          ; Define method to
        (setq total-area 0)                             ; re-initialize
        (setq field-area 0)))                           ; instance vari-
;:AREA                                                   ; ables.
(test (setq wiltshire (make-instance 'county)) "make-instance of county")
(test (test-stream-output "An instance of flavor COUNTY.
Instance variables:
TOTAL-AREA 0
NUMBER-OF-SPACES 10
FIELD-LENGTH 4
FIELD-WIDTH 2
FIELD-AREA 0
COUNT 0
FLOOR-LENGTH 5
FLOOR-WIDTH 3
FLOOR-AREA 0" (describe wiltshire)
 :print-stream *standard-output*) "describe an instance of county")

(test (equal '(150 80) (send wiltshire :area)) "test send county :area")
(test (test-stream-output "An instance of flavor COUNTY.
Instance variables:
TOTAL-AREA 0
NUMBER-OF-SPACES 10
FIELD-LENGTH 4
FIELD-WIDTH 2
FIELD-AREA 0
COUNT 1
FLOOR-LENGTH 5
FLOOR-WIDTH 3
FLOOR-AREA 15" (describe wiltshire) :print-stream *standard-output*)
      "test describe of county")



(compile-flavor-methods  circle-flavor  circle-flavor2  rectangle-flavor 
			 farmland-flavor circle-flavor3 a-flavor
			 new-flavor field keep-count farm factory county)

(kill-sub-lisp)
