;;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LUCID; Base: 10 -*-
;;;;
;;;; FLAV-BUG-TE, Module TEST
;;;
;;; ***************************************************************************
;;;
;;;    Copyright (c) 1987, 1988, 1989 by Lucid Inc.,  All Rights Reserved
;;;
;;; ***************************************************************************
;;;
;;; Tests for bugs in flavors
;;;
;;; Programmer: Jos Marlowe
;;;
;;; Edit-History:
;;;
;;; Created:   4-Mar-87 by Jos
;;;  9-Mar-87 pw: Added test for bug 1447.
;;;  6-Apr-87 moe: added test for bug 1828
;;; 30-Jun-87 moe: add tests for bugs 2029,2021,2018,2023,2102
;;; 31-Jul-87 meekie: moved test for bug 1308 from flav-eg-te.lisp
;;; 22-Sep-87 lnz: Include (setf (symbol-plist ...) ...) within eval-when.
;;; 12-Oct-87 moe: test for bug 2648
;;; 12-Oct-87 moe: test for bug 2658
;;; 27-Sep-88 moe: test for bug 3588
;;; 30-Sep-88 moe: test for bug 2414
;;; 17-Nov-88 moe: test for bug 4092
;;; 18-Nov-88 moe: fix test for bug 4092
;;;  1-May-89 moe: add test for 4526 and remove test for 2414 since 
;;;                the fix for it causes 4526.
;;;  4-May-89 moe: fix test for 4536
;;;  4-May-89 hardy: updated header, Module CLTEST==>TEST.
;;;
;;; End-of-Edit-History

(in-package 'user)

;;; Describe returns no values
;;;
(test (null (describe (progn (defflavor foo a b) (make-instance 'foo))))
      "bug 01447:  Inspector broken on flavor instances")

(defvar *tolerable-float* (* 5 short-float-epsilon))

(defun tolerable-value (x y)
    (< (abs (- x y)) *tolerable-float*))

(test (progn (defflavor :circle-flavor
    (x-center y-center radius)
    ()
    :initable-instance-variables
    :settable-instance-variables
    :gettable-instance-variables)

(defmethod (:circle-flavor :diameter) ()
    (* 2 radius))
(defmethod (:circle-flavor :area) ()
    (* pi radius radius))             ; pi is a built-in constant

(defmethod (:circle-flavor :moveright) (amount)
    (setq x-center (+ x-center amount)))

(defmethod (:circle-flavor :perimeter) ()
    (* pi (send self :diameter)))


;;; alternate definition of :circle-flavor.  Make sure we get the
;;; same results, and that it can be read back in.

(defflavor :circle-flavor2
    (x-center y-center diameter)
    ()
    :initable-instance-variables
    :settable-instance-variables
    :gettable-instance-variables)

(defmethod (:circle-flavor2 :radius) ()
    (/ diameter 2))

(defmethod (:circle-flavor2 :area) ()
    (* 1/4 pi diameter diameter))

(defmethod (:circle-flavor2 :moveright) (amount)
    (setq x-center (+ x-center amount)))

(let* ((random1 (random 100))
       (random2 (random 100))
       (random3 (random 100))
       (circle1 (make-instance ':circle-flavor
		      :x-center random1 :y-center random2 :radius random3))
       (circle2 (make-instance ':circle-flavor2
		      :x-center random1 :y-center random2
		      :diameter (* 2 random3))))
  (test (eql (send circle1 :x-center) (send circle2 :x-center))
	"testing equality of :circle-flavor, :circle-flavor2")
  (test (eql (send circle1 :y-center) (send circle2 :y-center))
	"ditto")
  (test (eql (send circle1 :radius) (send circle2 :radius))
	"ditto")
  (test (eql (send circle1 :diameter)(send circle2 :diameter))
	"ditto")
  (test (tolerable-value (send circle1 :area)(send circle2 :area))
	"ditto")))
  "see if this whole keyword  flavor mess works")

;;; bug 2029
(eval-when (eval compile load)

(setf (symbol-plist 'v7) '())

(setf (symbol-plist 'w7) '())

)

(test (progn
	(defflavor v7 ((iv1))
	  ()
	  :settable-instance-variables)

	(defflavor w7 ((iv1 'local-value)
		       (iv7))
	  (v7)
	  ))
      "set up for bug 2029 test")
(test (setq b (make-instance 'w7 :iv1 4)) "bug 2029 test")

;;; bug 2102

(eval-when (eval compile load)

(setf (symbol-plist 'first-flavor1) '())

(setf (symbol-plist 'second-flavor1) '())

)

(test (progn
	(defflavor first-flavor1 (a b c) ()
	  (:required-instance-variables d))

	(defflavor second-flavor1 (d) (first-flavor1)
	  )) "setup for bug 2102 test")

(test (make-instance 'second-flavor1) "bug 2102 test")

(eval-when (eval compile load)

(setf (symbol-plist 'mixin2) '())

(setf (symbol-plist 'required-test2) '())

)

(test (progn
	(defflavor mixin2 () ())

	(defflavor required-test2 () (mixin2) 
	  (:required-instance-variables iv2)))
      "setup for bug 2102")

(etest (make-instance 'required-test2) "bug 2102 test")

;;; bug 2023

(eval-when (eval compile load)

(setf (symbol-plist 'aaa1) '())

(setf (symbol-plist 'bbb1) '())

(setf (symbol-plist 'ccc1) '())

(setf (symbol-plist 'ddd1) '())

)

(test (progn
	(defflavor aaa1 ((property-list  nil)) () :settable-instance-variables)

	(defflavor bbb1 (name
			 (genname)
			 (gensepr "#")) ()
			 :settable-instance-variables)

	(defflavor ccc1 ((parent) (gensepr " #"))
	  (bbb1 aaa1) :settable-instance-variables)

	(defflavor ddd1 ((gensepr '|Object|) number)
	  (ccc1)
	  :settable-instance-variables))
      "setup for bug 2023 test")

(test (setq ti (make-instance 'ddd1 :property-list '(color blue))) "bug 2023")
(test (equal '(color blue) (send ti :property-list)) "bug 2023")

;;; bug 2021

(eval-when (eval compile load)

(setf (symbol-plist 'o1) '())

(setf (symbol-plist 't1) '())

)

(test (progn
	(defflavor o1 (nothing (val 1)) ())
	(defflavor t1 ((val 2) noval) (o1)
	  :settable-instance-variables))
"setup for bug 2021")

(test (setq ti (make-instance 't1)) "bug 2021")
(test (eq 2 (send ti :val)) "bug 2021")


;;; bug 2018

(eval-when (eval compile load)

(setf (symbol-plist 'mo) '())

)

(test (defflavor mo (a) nil) "setup for bug 2018 test")
(test (make-instance 'mo) "setup for bug 2018 test")
(test (defflavor mo ((a 2)) nil) "setup for bug 2018 test")
(test (setq ti (make-instance 'mo)) "setup for bug 2018 test")
(test (test-stream-output "An instance of flavor MO.
Instance variables:
A 2" (send ti :describe) :print-stream *standard-output*) "test bug 2018")



;;; bug #1308
  (let ()
    (defflavor loser
      (x y z)
      ()
      (:settable-instance-variables))
    (setq lose (make-instance 'loser))
    (test (send lose :set-x 1)
	  "bug #1308: :settable-instance-variables with no arguments"))


;;; bug 2648
(eval-when (eval compile load)
  (setf (symbol-plist 'test) nil)
  (setf (symbol-plist 'goo) nil)
  (setf (symbol-plist 'foo) nil))
(test (defflavor test ()()) "setup for bug 2648 test")
(test (setq i (make-instance 'test)) "setup for bug 2648 - make-instance")
(test (typep i 'test) "test of typep for flavor type")
(test (null (typep 12 'test)) "test of typep for flavor type on number")

(test (defflavor foo  () ()) "setup for testing flavors subtypes")
(test (defflavor goo () (foo) ) "setup for testing flavors subtypes")
(test (null (subtypep 'foo 'goo)) "test subtype on flavors -expect nil")
(test (subtypep 'goo 'foo) "test subtype on flavors - expect t")

;;; bug 2658

(eval-when (eval compile load)
  (setf (symbol-plist 'pl1) nil)
  (setf (symbol-plist 'test) nil))

(test (progn (setq plist-val '(1 2 3))
	     (defflavor test () () (:init-keywords :test)
			(:default-init-plist :test plist-val)))
      "defflavor of test with default-init-plist")
(test (defmethod (test :init) (plist)
	(print (getf plist :test))) "defmethod which uses test's plist")
(test (test-stream-output "(1 2 3)"
			  (make-instance 'test)
			  :print-stream *standard-output*) "test use of plist")

(test (defflavor pl (iv1 iv2) () 
	   (:default-init-plist :test plist-val :iv2 bound-var))
      "setup for plist test- pl")
(test (defmethod (pl :init) (plist)
	(print (getf plist :iv2)))
      "setup defmethod for pl")
(etest (setq i (make-instance 'pl)) "etest - unbound plist variable")

;;; bug 3588

(etest (test-stream-output ">>Error: Unknown defflavor option :OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES."
			  (defflavor base2
			      ((inherit :method)
			       (aggregate nil)
			       (parent nil)
			       (index nil)
			       (unique-id (gensym)))
			    ()
			    :initable-instance-variables
			    :gettable-instance-variables
			    :outside-accessible-instance-variables)
			  :print-stream *error-output*)
			  "bug 3588 test")
#|
;;; bug 2414
;;; dont do (setf (symbol-plist 'block)nil)!!
(test (progn (in-package 'user)
	     (defflavor block ()())
	     "setup for bug 2414 test"))
(test (find-symbol "BLOCK-INTERNAL-INIT-PRIMARY" "USER")
      "bug 2414")
(test (null (find-symbol "BLOCK-INTERNAL-INIT-PRIMARY" "LISP"))
      "bug 2414")
|#

;;; bug 2030

(eval-when (eval compile load)
  (mapc #'(lambda (x) (setf (symbol-plist x) nil))
	'(gennamed-object-mixin property-list-mixin :configuration-object)))

(test (defflavor gennamed-object-mixin
	  (name (genname)	;string to use instead of (type-of self)
   (gensepr "#"))
  () :settable-instance-variables)
      "test for bug 2030")

(test (defmethod (gennamed-object-mixin :after :init) (ignore)
	(setq name (genname (or genname (flavor-of-instance self)) gensepr)))
      "test for bug 2030")

(test (defwhopper (gennamed-object-mixin :print-self)
	  (stream prindepth slashify-p)
	prindepth%			; ignore
	(if name (format stream
			 (if slashify-p "~S" "~A") name)
	    (continue-whopper stream)))
      "test for bug 2030")


;;; gensym with atm- as prefix
(test (defun genname (atm &optional (sepr " ") num-form)
	(intern (string-append atm
			       (string sepr)
			       (format nil
				       "~@?"
				       (case num-form
					 ((:card :cardinal) "~R")
					 ((:ord :ordinal) "~:R")
					 ((:rom :roman) "~@R")
					 (otherwise "~D"))
				       (setf (get atm 'gennum)
					     (+ 1 (or (get atm 'gennum)
						      (setf (get atm 'gennum)
							    0))))))))
      "test for bug 2030")
(test (defflavor property-list-mixin
	  ((property-list (gensym "plist")))
	()
	:settable-instance-variables)
      "test for bug 2030")

(test (defmethod (property-list-mixin :get) (indicator)
	(get (send self :property-list) indicator))
      "test for bug 2030")

(test (defmethod (property-list-mixin :getl) (indicator-list)
	(get-properties
	  (symbol-plist (send self :property-list))
	  indicator-list))
      "test for bug 2030")

(test (defmethod (property-list-mixin :putprop) (value property)
	(setf (get (send self :property-list) property)
	      value))
      "test for bug 2030")

(test (defmethod (property-list-mixin :push-property) (value property)
	(let ((oldval (get (send self :property-list) property)))
	  (if (listp oldval)
	      (push value (get (send self :property-list) property))
	      (setf (get (send self :property-list) property)
		    (cons value (list oldval))))))
      "test for bug 2030")

(test (defmethod (property-list-mixin :remprop) (indicator)
	(remprop (send self :property-list) indicator))
      "test for bug 2030")
(test (defmethod (property-list-mixin :plist) ()
	(symbol-plist (send self :property-list)))
      "test for bug 2030")

(test (defflavor :configuration-object
	  ((parent)				;generic uplink
	   (gensepr " #"))
	(gennamed-object-mixin property-list-mixin)
	:settable-instance-variables)
      "test for bug 2030")


(eval-when (eval compile load)
  (setf (symbol-plist 'symbol) nil))
(etest (defflavor symbol ()()) "setup for bug 4092 test")

(in-package 'user)
(test (progn
	(unless (find-package "D")
	  (make-package "D" :use '(lisp lucid-common-lisp user))
	  (when *hacked-defmethod*
	    (shadowing-import '(flavors::defmethod
				flavors::undefmethod
				flavors::make-instance) "D")
	    (use-package 'flavors "D")))
	(unless (find-package "C")
	  (make-package "C" :use nil))
	T) "setup for bug 4536")
(eval-when (eval compile load)
	(setf (symbol-plist 'd::foo) nil)
	(setf (symbol-plist 'c::foo) nil)
	(setf (symbol-plist 'c::foo1) nil)	)

(test (progn
	(defflavor c::foo ((v1 2))())  
	(defflavor d::foo ((v2 3))())  
	(setq *aaa* (make-instance 'c::foo))
	(defmethod (c::foo :bar) () v1);; foo-bar-primary is in package "D"
	(defmethod (d::foo :bar) () v2)
	);;foo-bar-primary is also in package "D"
      "setup for testing bug 4526")
(test (send *aaa* :bar) "test bug 4526")

(test (progn
	(defflavor c::foo1 ((v1 2))()) 
	(defflavor d::foo1 ((v2 3))())
	(defmethod (c::foo1 :bar) () v1)
	(defmethod (d::foo1 :bar) () v2)
	(setq *aaa* (make-instance 'c::foo1))
	(send *aaa* :bar)
	(setq d (make-instance 'd::foo1))
	(send d :bar)
	)
      "test bug 4526")

