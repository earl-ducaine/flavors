;;; -*- Mode: LISP; Syntax: Common-lisp; Package: FLAVORS -*-
;;; 
;;; def-opts.lisp, Module FLAVORS
;;;
;;; ********************************************************************
;;;
;;;  (c) Copyright 1986, 1987, 1988 by Lucid Inc., All Rights Reserved
;;;
;;; ********************************************************************
;;;
;;; Edit-History: test file for :gettable, :initable, and
;;; :settable-instance-variables 
;;;
;;; Created: moe 1/4/86
;;; Reviewed: <Review-Date>
;;; 27-Jan-86 moe: removed (in-packge 'flavors) ,use (use-package 'flavors)
;;; 28-Jan-86 moe: don't try to import lucid:test nor  etest
;;; 19-Nov-86 lnz: flavors-system ==> flavor-internals.
;;; 22-Sep-87 lnz: Include (setf (symbol-plist ...) ...) within eval-when.
;;; 18-Nov-87 lnz: Removed format call.
;;;  3-Nov-88 hardy: added filename & Module to header, updated 
;;;                  copyright notice
;;;
;;; End-of-Edit-History
 
(use-package 'flavors)

;;; make sure we are starting fresh

(eval-when (eval compile load)

(setf (symbol-plist 'mixin) '())
(setf (symbol-plist 'mixin1) '())
(setf (symbol-plist 'simple-get-test) '())
(setf (symbol-plist 'get-test) '())
(setf (symbol-plist 'get-test-2) '())
(setf (symbol-plist 'set-test) '())
(setf (symbol-plist 'set-test-2) '())
(setf (symbol-plist 'simple-set-test) '())
(setf (symbol-plist 'init-test) '())
(setf (symbol-plist 'init-test-2) '())
(setf (symbol-plist 'simple-init-test) '())
(setf (symbol-plist 'i-mixin) '())
(setf (symbol-plist 'i-mixin1) '())
(setf (symbol-plist 's-mixin) '())
(setf (symbol-plist 's-mixin1) '())

)

(defflavor mixin ((mixin-iv "mixin-iv") (mixin-iv1 "mixin-iv1")) ()
	   :gettable-instance-variables)

(defflavor mixin1 ((mixin1-iv-1 "mixin1-iv-1") (mixin1-iv-2 "mixin1-iv-2")) ()
	   )

(defflavor simple-get-test ((g-test-iv "g-test-iv") (g-test-iv-1 "g-test-iv-1")
			    g-test-unbound)
  ()
  :gettable-instance-variables)

(defflavor get-test ((g-test-iv "g-test-iv"))(mixin)
	   :gettable-instance-variables)

(defflavor get-test-2 ((gettable-iv "gettable-iv") ungettable-iv
		     mixin1-iv-1 mixin1-iv-2) (mixin1)
	   (:gettable-instance-variables gettable-iv mixin1-iv-1 mixin1-iv-2))

(defflavor i-mixin (mixin-iv  mixin-iv1 ) ()
	   :initable-instance-variables
	   :gettable-instance-variables)

(defflavor i-mixin1 (mixin1-iv (mixin1-iv-2 "mixin1-iv-2")) ()
	   :gettable-instance-variables)

(defflavor simple-init-test ((i-test-iv "i-test-iv")
			     (i-test-iv-1 "i-test-iv-1")
			     i-test-iv-2)
  ()
  :initable-instance-variables
  :gettable-instance-variables)

(defflavor init-test (g-test-iv)(i-mixin)
	   :initable-instance-variables
	   :gettable-instance-variables)

(defflavor init-test-2 (g-test-iv mixin1-iv uninitable-iv)(mixin1)
	   (:initable-instance-variables g-test-iv mixin1-iv)
	   :gettable-instance-variables)

(defflavor simple-set-test ((s-test-iv "s-test-iv")
			    (s-test-iv-1 "s-test-iv-1")
			    s-test-iv-2)
  ()
   :settable-instance-variables)

(defflavor s-mixin ((mixin-iv "mixin-iv") (mixin-iv1 "mixin-iv1")) ()
	   :settable-instance-variables)

(defflavor s-mixin1 ((mixin1-iv-1 "mixin1-iv-1")
		     (mixin1-iv-2 "mixin1-iv-2"))
  ()
	   )

(defflavor set-test (s-test-iv)(s-mixin)
	   :settable-instance-variables)

(defflavor set-test-2 (s-test-iv mixin1-iv mixin1-iv-2)(mixin1)
	   (:settable-instance-variables mixin1-iv)
	   (:gettable-instance-variables s-test-iv))

(defun defflavor-options-test ()
  (setq *test-instance* (make-instance 'simple-get-test))
  (test (equal '("g-test-iv" "g-test-iv-1") (list (send *test-instance*
							:g-test-iv)
						  (send *test-instance*
							:g-test-iv-1)))
	"simple get test")
  (test (equal 'flavor-internals::unbound (send *test-instance*
					      :g-test-unbound))
	"get an unbound instance variable")
  (etest (send *test-instance* :g-test-misspelled)
	 "try to get a non existent iv")
  (setq *test-instance* (make-instance 'get-test))
  (test (equal '("g-test-iv" "mixin-iv" "mixin-iv1")
	       (list (send *test-instance* :g-test-iv)
		     (send *test-instance* :mixin-iv)
		     (send *test-instance* :mixin-iv1)))
	"test get-iv with mixins")
  (setq *test-instance* (make-instance 'get-test-2))
  (etest (send *test-instance* :ungettable-iv)
	 "etest for ungettable iv")
  (test (equal '("gettable-iv" "mixin1-iv-1"  "mixin1-iv-2")
	       (list (send *test-instance* :gettable-iv)
		     (send *test-instance* :mixin1-iv-1)
		     (send *test-instance* :mixin1-iv-2)))
	"test gettable-iv for mixin with :get option specified by test fl")
  (etest (send *test-instance* :gettable-iv-misspelled)
	 "etest for misspelled iv")
  (setq *test-instance* (make-instance 'simple-init-test
				       :i-test-iv "something-new"
				       :i-test-iv-2 "something-else-new"))
  (test (equal '("something-new" "i-test-iv-1" "something-else-new")
	       (list (send *test-instance* :i-test-iv)
		     (send *test-instance* :i-test-iv-1)
		     (send *test-instance* :i-test-iv-2)))
	"initialize and get simple ivs")
  (etest (make-instance 'simple-init-test :unknown-init)
	 "etest of unknown init")
  (setq *test-instance* (make-instance 'simple-init-test))
  (test (equal '("i-test-iv" "i-test-iv-1" flavor-internals::unbound)
	       (list (send *test-instance* :i-test-iv)
		     (send *test-instance* :i-test-iv-1)
		     (send *test-instance* :i-test-iv-2)))
	"test default initializations")
  (setq *test-instance* (make-instance 'init-test
				       :g-test-iv "g-test-iv"
				       :mixin-iv "mixin-iv"
				       :mixin-iv1 "mixin-iv1"))
  (test (equal '("g-test-iv" "mixin-iv" "mixin-iv1")
	       (list (send *test-instance* :g-test-iv)
		     (send *test-instance* :mixin-iv)
		     (send *test-instance* :mixin-iv1)))
	"test initable ivs with mixins")
  (setq *test-instance* (make-instance 'init-test-2
				       :g-test-iv "g-test-iv"
				       :mixin1-iv "mixin1-iv"))
  (test (equal '("mixin1-iv" "g-test-iv")
	       (list (send *test-instance* :mixin1-iv)
		     (send *test-instance* :g-test-iv)))
	"test init options with mixins")
  (etest (make-instance 'init-test :uninitable-iv 3)
	 "etest for uninitable iv")
  (setq *test-instance* (make-instance 'simple-set-test :s-test-iv
				       "something-new"))
  (test (equal '("something-new" "s-test-iv-1")
	       (list (send *test-instance* :s-test-iv)
		     (send *test-instance* :s-test-iv-1)))
	"simple initialize and get test for set option")
  (test (equal "something-else" (send *test-instance* :set-s-test-iv
				      "something-else"))
	"simple set test")
  (test (equal "something-else" (send *test-instance* :s-test-iv))
	"get a settable iv")
  (etest (send *test-instance* :set-unknown)
	 "etest for setting unknown iv")
  (etest (send *test-instance* :get-unknown)
	 "etest for getting unknown iv")
  (etest (setq *test-instance* (make-instance 'simple-set-test :unknown))
	 "etest for initing unknown iv")
  (setq *test-instance* (make-instance 'set-test
				       :s-test-iv "s-test-iv"
				       :mixin-iv "s-mixin-iv"
				       :mixin-iv1 "s-mixin-iv1"))
  (test (equal '("s-test-iv" "s-mixin-iv" "s-mixin-iv1")
	       (list (send *test-instance* :s-test-iv)
		     (send *test-instance* :mixin-iv)
		     (send *test-instance* :mixin-iv1)))
	"get test for settable ivs with mixin")
  (test (equal "something-new" (send *test-instance* :set-mixin-iv
				     "something-new"))
	"set test for settable ivs with mixins")
  (test (equal "something-new" (send *test-instance* :mixin-iv))
	"set test for settable ivs with mixins")
  (setq *test-instance* (make-instance 'set-test-2 :mixin1-iv "something-new"))
  (test (equal "something-new" (send *test-instance* :mixin1-iv))
	"set test for some settable ivs with mixins")
  (test (equal "something-else" (send *test-instance* :set-mixin1-iv
				      "something-else"))
	"set test for some settable ivs with mixins")
  (etest (make-instance 'set-test-2 :s-test-iv 3)
	 "etest for unsettable iv"))

(compile-flavor-methods  mixin mixin1 simple-get-test get-test
			 get-test-2 i-mixin i-mixin1 simple-init-test
			 init-test init-test-2 simple-set-test
			 s-mixin s-mixin1 set-test set-test-2)

(defflavor-options-test)
