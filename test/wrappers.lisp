;;; -*- Mode: LISP; Syntax: Common-lisp; Package: FLAVORS -*-
;;;
;;; ******************************************************************
;;;
;;; (c) Copyright 1986 by Lucid Inc., All Rights Reserved
;;;
;;; ******************************************************************
;;;
;;; Edit-History:test file for wrappers and whoppers.  Also tests undefmethod.
;;;
;;; Created: 11/20/85
;;; Reviewed: <Review-Date>
;;;
;;; 27-Jan-86 moe: removed (in-packge 'flavors) ,use (use-package 'flavors)
;;; 28-Jan-86 moe: don't try to import lucid:test nor  etest
;;; 28-Jan-86 moe: change foo to fooo and bar to baar to get around
;;;                redefining bug
;;;  3-Feb-86 moe: remove (setf (symbol-plist...)) since flavor names 
;;;                unique.
;;; 19-Mar-87 susan: modified to not run in application environment
;;;                  since does undefmethods within functions
;;;  8-Apr-87 tomh: fixed broken-eval stuff
;;; 18-Nov-87 lnz: Removed format call.
;;;
;;; End-of-Edit-History

(in-package 'flavors)

(defvar *test-list* ()) 

(defvar *test-instance*)

(defvar *f*)

(defvar *something*)

(defflavor fooo () ())

(defmethod (fooo :add-something) (n)
  (+ *something* n))
(defmethod (fooo :before :add-something) (n)
  (setq *test-list* (cons "before-add-something" *test-list*)))
(defmethod (fooo :after :add-something ) (n)
  (setq *test-list* (cons "after-add-something" *test-list*)))
(defwrapper (fooo :add-something) (ignore . body)
  `(progn (setq *test-list* ())
	  . ,body))
(defwhopper (fooo :add-something) (n)
  (progn (setq  *something* 3)
	 (continue-whopper n)))

(defun simple-wh-test()
  (setq *f* (make-instance 'fooo))
  (test (eq 6 (send *f* :add-something 3)) "simple whopper test")
  (test (equal '("after-add-something" "before-add-something")
	       *test-list*) "test before and afters with wrappers")
  (test (eq 5 (send *f* :add-something 2)) "simple whopper test")
  (test (equal '("after-add-something" "before-add-something")
	       *test-list*) "test before and afters with wrappers")
  (setq *test-list* '("undefine-wrapper"))
  (undefmethod (fooo :wrapper :add-something))
  (test (eq 3 (send *f* :add-something 0)) "undefine a wrapper")
  (test (equal '("after-add-something" "before-add-something" "undefine-wrapper")
	       *test-list*) "undefine wrapper")
  (setq *something* 0)
  (undefmethod (fooo :whopper :add-something))
  (test (eq 0 (send *f* :add-something 0)) "undefine a whopper")
  (undefmethod (fooo :before :add-something))
  (setq *test-list* ())
  (test (eq 1 (send *f* :add-something 1)) "undefine before method")
  (test (equal '("after-add-something") *test-list*) "undefine before method")
  (undefmethod (fooo :after :add-something))
  (setq *test-list* ())
  (test (eq 1 (send *f* :add-something 1)) "undefine after method")
  (test (null *test-list*) "undefine after method")
  (undefmethod (fooo :add-something))
  (etest (send *f* :add-something 0) "all methods undefined"))

(defvar *test-list* ())
(defflavor fooo1 () ())
(defmethod (fooo1 :add-something) (&rest n)
  (+ *something* (car n)))
(defmethod (fooo1 :before :add-something) (&rest n)
  (setq *test-list* (cons "before-add-something" *test-list*)))
(defmethod (fooo1 :after :add-something ) (&rest n)
  (setq *test-list* (cons "after-add-something" *test-list*)))
(defwrapper (fooo1 :add-something) (ignore . body)
  `(progn (setq *test-list* ())
	  . ,body))
(defwhopper (fooo1 :add-something) (&rest n)
  (progn (setq  *something* 3)
	 (lexpr-continue-whopper n)))

(defmethod (fooo1 :add-something-else) (&rest n)
  (+ *something* (car n)))
(defmethod (fooo1 :before :add-something-else) (&rest n)
  (setq *test-list* (cons "before-add-something-else" *test-list*)))
(defmethod (fooo1 :after :add-something-else ) (&rest n)
  (setq *test-list* (cons "after-add-something-else" *test-list*)))
(defwrapper (fooo1 :add-something-else) (ignore . body)
  `(progn (setq *test-list* ())
	  . ,body))
(defwhopper (fooo1 :add-something-else) (&rest n)
  (progn (setq  *something* 3)
	 (continue-whopper-all )))


(defun lexper-wh-test()
  (setq *f* (make-instance 'fooo1))
  (test (eq 6 (send *f* :add-something 3)) "simple whopper test")
  (test (equal '("after-add-something" "before-add-something")
	       *test-list*) "test before and afters with wrappers")
  (test (eq 5 (send *f* :add-something 2)) "simple whopper test")
  (test (equal '("after-add-something" "before-add-something")
	       *test-list*) "test before and afters with wrappers")
  (setq *test-list* '("undefine-wrapper"))
  (undefmethod (fooo1 :wrapper :add-something))
  (test (eq 3 (send *f* :add-something 0)) "undefine a wrapper")
  (test (equal '("after-add-something" "before-add-something" "undefine-wrapper")
	       *test-list*) "undefine wrapper")
  (setq *something* 0)
  (undefmethod (fooo1 :whopper :add-something))
  (test (eq 0 (send *f* :add-something 0)) "undefine a whopper")
  (undefmethod (fooo1 :before :add-something))
  (setq *test-list* ())
  (test (eq 1 (send *f* :add-something 1)) "undefine before method")
  (test (equal '("after-add-something") *test-list*) "undefine before method")
  (undefmethod (fooo1 :after :add-something))
  (setq *test-list* ())
  (test (eq 1 (send *f* :add-something 1)) "undefine after method")
  (test (null *test-list*) "undefine after method")
  (undefmethod (fooo1 :add-something))
  (etest (send *f* :add-something 0) "all methods undefined"))
(defun continue-wh-test ()
  (test (eq 6 (send *f* :add-something-else 3)) "simple whopper test")
  (test (equal '("after-add-something-else" "before-add-something-else")
	       *test-list*) "test before and afters with wrappers")
  (test (eq 5 (send *f* :add-something-else 2)) "simple whopper test")
  (test (equal '("after-add-something-else" "before-add-something-else")
	       *test-list*) "test before and afters with wrappers")
  (setq *test-list* '("undefine-wrapper"))
  (undefmethod (fooo1 :wrapper :add-something-else))
  (test (eq 3 (send *f* :add-something-else 0)) "undefine a wrapper")
  (test (equal '("after-add-something-else" "before-add-something-else" "undefine-wrapper")
	       *test-list*) "undefine wrapper")
  (setq *something* 0)
  (undefmethod (fooo1 :whopper :add-something-else))
  (test (eq 0 (send *f* :add-something-else 0)) "undefine a whopper")
  (undefmethod (fooo1 :before :add-something-else))
  (setq *test-list* ())
  (test (eq 1 (send *f* :add-something-else 1)) "undefine before method")
  (test (equal '("after-add-something-else") *test-list*) "undefine before method")
  (undefmethod (fooo1 :after :add-something-else))
  (setq *test-list* ())
  (test (eq 1 (send *f* :add-something-else 1)) "undefine after method")
  (test (null *test-list*) "undefine after method")
  (undefmethod (fooo1 :add-something-else))
  (etest (send *f* :add-something-else 0) "all methods undefined"))

;;; test whopper construction with mixins
(defflavor baar-mixin () ())
(defmethod (baar-mixin :make-list) (n)
  (setq *test-list* (cons n (cons "baar-mixin-make-list" *test-list*))))
(defmethod (baar-mixin :before :make-list) (n)
  (setq *test-list* (cons n (cons "baar-mixin-before-make-list" *test-list*))))
(defmethod (baar-mixin :after :make-list ) (n)
  (setq *test-list* (cons n (cons "baar-mixin-after-make-list" *test-list*))))
(defwrapper (baar-mixin :make-list) (ignore . body)
  `(progn (setq *test-list* (cons "baar-mixin-wrapper" *test-list*))
	  . ,body))
(defwhopper (baar-mixin :make-list) (n)
  (cond ((< n 5)
	 (continue-whopper n))
	((>= n 5)
	 (continue-whopper 5))))
(defflavor baar () (baar-mixin))
(defmethod (baar :make-list) (n)
  (setq *test-list* (cons n (cons "baar-make-list" *test-list*))))
(defmethod (baar :before :make-list) (n)
  (setq *test-list* (cons n (cons "baar-before-make-list" *test-list*))))
(defmethod (baar :after :make-list ) (n)
  (setq *test-list* (cons n (cons "baar-after-make-list" *test-list*))))
(defwrapper (baar :make-list) (ignore . body)
  `(progn (setq *test-list* ())
	  . ,body))
(defwhopper (baar :make-list) (n)
  (cond ((>= n 5)
	 (continue-whopper 5))
	((< n 5)
	 (continue-whopper n))
	))
(defun mixin-wh-test ()
  (setq *test-list* ())
  (setq *test-instance* (make-instance 'baar))
  (test (equal '(3 "baar-make-list" 3 "baar-mixin-before-make-list"
		 3 "baar-before-make-list" "baar-mixin-wrapper")
	       (send *test-instance* :make-list 3)) "mixin whopper test")
  
  (test (equal '(3 "baar-after-make-list" 3 "baar-mixin-after-make-list"
		 3 "baar-make-list" 3 "baar-mixin-before-make-list" 3
		 "baar-before-make-list" "baar-mixin-wrapper")
	       *test-list*) "test before and afters with mixin wrappers")
  (test (equal '(5 "baar-make-list" 5 "baar-mixin-before-make-list"
		 5 "baar-before-make-list" "baar-mixin-wrapper")
	       (send *test-instance* :make-list 6)) "simple whopper test")
  (test (equal '(5 "baar-after-make-list" 5 "baar-mixin-after-make-list"
		 5 "baar-make-list" 5 "baar-mixin-before-make-list" 5
		 "baar-before-make-list" "baar-mixin-wrapper")
	       *test-list*) "test before and afters with mixin wrappers")
  (setq *test-list* '(undefine-wrapper))
  (undefmethod (baar-mixin :wrapper :make-list))
  (test (equal '(0 "baar-make-list" 0 "baar-mixin-before-make-list" 0
		 "baar-before-make-list")
	       (send *test-instance* :make-list 0)) "undefine a wrapper")
  (test (equal '(0 "baar-after-make-list" 0 "baar-mixin-after-make-list"
		 0 "baar-make-list" 0 "baar-mixin-before-make-list"
		 0 "baar-before-make-list")
	       *test-list*) "undefine mixin wrapper")
  (undefmethod (baar-mixin :whopper :make-list))
  (test (equal '(0 "baar-make-list" 0 "baar-mixin-before-make-list"
		 0 "baar-before-make-list")
	       (send *test-instance* :make-list 0)) "undefine a mixin whopper")
  (test (equal '(0 "baar-after-make-list" 0 "baar-mixin-after-make-list"
		 0 "baar-make-list" 0 "baar-mixin-before-make-list" 0
		 "baar-before-make-list") *test-list* ) "undefine a mixin whopper")
  (undefmethod (baar-mixin :before :make-list))
  (test (equal '(1 "baar-make-list" 1 "baar-before-make-list")
	       (send *test-instance* :make-list 1))
	"undefine before mixin method")
  (test (equal '(1 "baar-after-make-list" 1 "baar-mixin-after-make-list"
		 1 "baar-make-list" 1 "baar-before-make-list") *test-list*)
	"undefine before mixin method")
  (undefmethod (baar-mixin :after :make-list))
  (test (equal '(5 "baar-make-list" 5 "baar-before-make-list")
	       (send *test-instance* :make-list 6))
	"undefine after mixin method")
  (test (equal '(5 "baar-after-make-list" 5 "baar-make-list" 5
		 "baar-before-make-list") *test-list*)
	"undefine after mixin method")
  (undefmethod (baar-mixin :make-list))
  (test (equal '(0 "baar-make-list" 0 "baar-before-make-list")
	       (send *test-instance* :make-list 0))
	"all mixin methods undefined"))
(defun base-wh-test ()
  (setq *test-list* '("undefine-wrapper"))
  (undefmethod (baar :wrapper :make-list))
  (test (equal '(0 "baar-make-list" 0 "baar-before-make-list" "undefine-wrapper")
	       (send *test-instance* :make-list 0)) "undefine base wrapper")
  (test (equal '(0 "baar-after-make-list" 0 "baar-make-list"
		 0 "baar-before-make-list" "undefine-wrapper")
	       *test-list*) "undefine base wrapper")
  (undefmethod (baar :whopper :make-list))
  (setq *test-list* ())
  (test (equal '(0 "baar-make-list" 0 "baar-before-make-list")
	       (send *test-instance* :make-list 0)) "undefine a mixin whopper")
  (test (equal '(0 "baar-after-make-list" 0 "baar-make-list"
		 0 "baar-before-make-list") *test-list* )
	"undefine a base whopper")
  (setq *test-list* ())
  (undefmethod (baar :before :make-list))
  (test (equal '(1 "baar-make-list")
	       (send *test-instance* :make-list 1))
	"undefine before base method")
  (test (equal '(1 "baar-after-make-list" 1 "baar-make-list") *test-list*)
	"undefine before base method")
  (setq *test-list* ())
  (undefmethod (baar :after :make-list))
  (test (equal '(6 "baar-make-list")
	       (send *test-instance* :make-list 6))
	"undefine after base method")
  (test (equal '(6 "baar-make-list") *test-list*)
	"undefine after base method")
  (undefmethod (baar :make-list))
  (etest (send *test-instance* :make-list 0)
	 "all methods undefined"))

(simple-wh-test)
(lexper-wh-test)
(continue-wh-test)
(mixin-wh-test)
(base-wh-test)
