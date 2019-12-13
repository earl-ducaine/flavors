;;; -*- Mode: LISP; Syntax: Common-lisp; Package: FLAVORS -*-
;;;
;;; daemon-cm.lisp, Module FLAVORS
;;;
;;; ***********************************************************************
;;;
;;;   (c) Copyright 1986, 1987, 1988 by Lucid Inc., All-Rights Reserved
;;;
;;; ***********************************************************************
;;;
;;; Test file for :daemon method combination.
;;;
;;; Edit-History:
;;;
;;; Created: 11/12/85 by moe
;;; Reviewed: <Review-Date>
;;; 27-Jan-86 moe: removed (in-packge 'flavors) ,use (use-package 'flavors)
;;; 28-Jan-86 moe: don't try to import lucid:test nor  etest
;;; 22-Sep-87 lnz: Include (setf (symbol-plist ...) ...) within eval-when.
;;; 18-Nov-87 lnz: Removed format call.
;;;  3-Nov-88 hardy: added filename & Module to header, updated
;;;                  copyright notice
;;;
;;; End-of-Edit-History

(use-package 'flavors)

(eval-when (eval compile load)

(setf (symbol-plist 'd-flavor-1) '())

(setf (symbol-plist 'd-flavor-2) '())

(setf (symbol-plist 'd-flavor-3) '())

(setf (symbol-plist 'd-flavor-4) '())

)

(defvar *test-instance*)

(defvar *test-list* '())

;;; test with multiple mixins

(defflavor d-flavor-1 ((flavor-1-iv "flavor-1-iv")) ()
  :gettable-instance-variables)

(defmethod (d-flavor-1 :make-lists) ()
  (setq *test-list* (cons flavor-1-iv *test-list*  )))

(defmethod (d-flavor-1 :before :make-lists) ()
  (setq *test-list* (cons "before-flavor-1" *test-list*)))

(defmethod (d-flavor-1 :after :make-lists)()
  (setq *test-list* (cons "after-flavor-1" *test-list*)))

(defmethod (d-flavor-1 :make-lists-2) ()
  (setq *test-list* (cons flavor-1-iv *test-list*  )))

(defmethod (d-flavor-1 :before :make-lists-2) ()
  (setq *test-list* (cons "before-flavor-1" *test-list*  )))

(defmethod (d-flavor-1 :after :make-lists-2) ()
  (setq *test-list* (cons "after-flavor-1" *test-list*  )))

(defmethod (d-flavor-1 :make-lists-3) ()
  (setq *test-list* (cons flavor-1-iv *test-list*  )))

(defmethod (d-flavor-1 :make-lists-4) ()
  (setq *test-list* (cons flavor-1-iv *test-list*  )))

(defflavor d-flavor-3  ((flavor-3-iv "flavor-3-iv") )
	   ())

(defmethod (d-flavor-3  :make-lists) ()
  (setq *test-list* (cons  flavor-3-iv *test-list* ))
  )

(defmethod (d-flavor-3 :before :make-lists) ()
  (setq *test-list* (cons "before-flavor-3" *test-list*))
  )

(defmethod (d-flavor-3 :after :make-lists) ()
  (setq *test-list* (cons "after-flavor-3" *test-list*))
  )

(defmethod (d-flavor-3  :make-lists-2) ()
  (setq *test-list* (cons  flavor-3-iv *test-list* ))
  )

(defmethod (d-flavor-3  :before :make-lists-2) ()
  (setq *test-list* (cons  "before-flavor-3" *test-list* ))
  )

(defmethod (d-flavor-3  :after :make-lists-2) ()
  (setq *test-list* (cons  "after-flavor-3" *test-list* ))
  )

(defmethod (d-flavor-3  :make-lists-3) ()
  (setq *test-list* (cons  flavor-3-iv *test-list* ))
  )

(defmethod (d-flavor-3  :make-lists-4) ()
  (setq *test-list* (cons  flavor-3-iv *test-list* ))
  )

(defmethod (d-flavor-3  :before :make-lists-4) ()
  (setq *test-list* (cons  "before-flavor-3" *test-list* ))
  )

(defmethod (d-flavor-3  :after :make-lists-4) ()
  (setq *test-list* (cons  "after-flavor-3" *test-list* ))
  )

(defflavor d-flavor-2 ((flavor-2-iv "flavor-2-iv") ) (d-flavor-1 d-flavor-3)
   :gettable-instance-variables	   )

(defmethod (d-flavor-2  :make-lists) ()
  (setq *test-list* (cons  flavor-2-iv *test-list* ))
  )

(defmethod (d-flavor-2  :before :make-lists) ()
  (setq *test-list* (cons  "before-flavor-2" *test-list* ))
  )

(defmethod (d-flavor-2  :after :make-lists) ()
  (setq *test-list* (cons  "after-flavor-2" *test-list* ))
  )

(defmethod (d-flavor-2  :make-lists-2) ()
  (setq *test-list* (cons  flavor-2-iv *test-list* ))
  )

(defmethod (d-flavor-2  :before :make-lists-2) ()
  (setq *test-list* (cons  "before-flavor-2" *test-list* ))
  )

(defmethod (d-flavor-2  :after :make-lists-2) ()
  (setq *test-list* (cons  "after-flavor-2" *test-list* ))
  )

(defmethod (d-flavor-2  :make-lists-3) ()
  (setq *test-list* (cons  flavor-2-iv *test-list* ))
  )

(defmethod (d-flavor-2  :make-lists-4) ()
  (setq *test-list* (cons  flavor-2-iv *test-list* ))
  )

(defflavor d-flavor-4 ((flavor-4-iv "flavor-4-iv") )
  (d-flavor-2) 
  (:method-combination (:daemon :base-flavor-last :make-lists)
                       (:daemon :base-flavor-first :make-lists-2)
		       (:daemon :base-flavor-first :make-lists-3)
		       (:daemon :base-flavor-last :make-lists-4)
   )
  :gettable-instance-variables)

(defmethod (d-flavor-4  :make-lists) ()	
  (setq *test-list* (cons  flavor-4-iv *test-list* )))

(defmethod (d-flavor-4  :before :make-lists) ()	
  (setq *test-list* (cons  "before-flavor-4" *test-list* )))

(defmethod (d-flavor-4  :after :make-lists) ()	
  (setq *test-list* (cons  "after-flavor-4" *test-list* )))

(defmethod (d-flavor-4  :make-lists-2) ()	
  (setq *test-list* (cons  flavor-4-iv *test-list* )))

(defmethod (d-flavor-4  :before :make-lists-2) ()	
  (setq *test-list* (cons  "before-flavor-4" *test-list* )))

(defmethod (d-flavor-4  :after :make-lists-2) ()	
  (setq *test-list* (cons  "after-flavor-4" *test-list* )))

(defmethod (d-flavor-4  :make-lists-3) ()	
  (setq *test-list* (cons  flavor-4-iv *test-list* )))

(defmethod (d-flavor-4  :make-lists-4) ()	
  (setq *test-list* (cons  flavor-4-iv *test-list* )))

(defun multiple-daemon-test ()
  (setq *test-instance* (make-instance 'd-flavor-4))
  (setq *test-list* ())
  (test (and (equal '("flavor-4-iv" "before-flavor-3" "before-flavor-1"
		      "before-flavor-2" "before-flavor-4")
		    (send *test-instance* :make-lists))
	     (equal *test-list* '("after-flavor-4" "after-flavor-2"
				  "after-flavor-1" "after-flavor-3"
				  "flavor-4-iv" "before-flavor-3"
				  "before-flavor-1" "before-flavor-2"
				  "before-flavor-4")))
	":daemon :base-flavor-last test with befores and afters")
  (setq *test-list* ())
  (test (and (equal '("flavor-4-iv" "before-flavor-3" "before-flavor-1"
		      "before-flavor-2" "before-flavor-4")
		    (send *test-instance* :make-lists-2))
	     (equal *test-list* '("after-flavor-4" "after-flavor-2"
				  "after-flavor-1" "after-flavor-3"
				  "flavor-4-iv" "before-flavor-3"
				  "before-flavor-1" "before-flavor-2"
				  "before-flavor-4")))
	":daemon :base-flavor-first test with befores and afters")
  (setq *test-list* ())
  (test (and (equal '("flavor-4-iv")(send *test-instance* :make-lists-3))
	     (equal '("flavor-4-iv") *test-list*))
	"primary methods only,:base-flavor-first")
  (setq *test-list* ())
  (test (and (equal '("flavor-4-iv" "before-flavor-3" )
		    (send *test-instance* :make-lists-4))
	     (equal'("after-flavor-3" "flavor-4-iv" "before-flavor-3")
	       *test-list*))
	"before and after in flavor-3,:base-flavor-last"))

(compile-flavor-methods d-flavor-1 d-flavor-3 d-flavor-2  d-flavor-4)

(multiple-daemon-test)
