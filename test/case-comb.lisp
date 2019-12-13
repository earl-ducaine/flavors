;;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LUCID; Base: 10 -*-
;;;;
;;;; case-comb.lisp, Module FLAVORS
;;;
;;; ***************************************************************************
;;;
;;;        Copyright (C) 1987, 1988 by Lucid, Inc.  All Rights Reserved
;;;
;;; ***************************************************************************
;;;
;;; test for new feature, :case combination
;;;
;;; Programmer: Shirley J. Kumamoto
;;;
;;; Edit-History:
;;;
;;; Created:  5-Oct-87 by moe
;;;  6-Oct-87 lnz: Fixed typo.
;;;  9-Feb-88 moe: add test for bug-3189
;;;  3-Nov-88 hardy: corrected filename in header, updated copyright
;;;                  notice, changed Module from TEST/FLAVORS to FLAVORS
;;;
;;; End-of-Edit-History

(eval-when (eval load compile)
(mapc #'(lambda (x) (setf (symbol-plist x) nil))
      `(a1 a2 a3 a4 a5 a6 a7 a8 a9
	   b1 b2 b9 b10 b8 bb3 aa3
	   grocer green-grocer ordinary exotica)))


;;; legal
(defflavor a1 ()()(:method-combination (:case :base-flavor-first :foon)))
(defmethod (a1 :case :foon :case1) () () (format t "~% a fooon case1"))
(defmethod (a1 :case :foon :case2) () () (format t "~% a fooon case2"))
(test (setq i (make-instance 'a1)) "make-instance of :case flavor")
(test (test-stream-output " a fooon case1"
			   (send i :foon :case1)
			   :print-stream *standard-output*)
      "test sending case message in flavor with multiple case statements")
(test (test-stream-output " a fooon case2" (send i :foon :case2)
			  :print-stream *standard-output*)
      "test sending case message in flavor with multiple case statements")

;;; illegal
(defflavor a2 ()()(:method-combination (:case :base-flavor-first :foon)))
(defmethod (a2  :foon :case1) () () (format t "~% a fooon case1"))
(defmethod (a2  :foon :case2) () () (format t "~% a fooon case2"))
(test (setq i (make-instance 'a2)) "make-instance of a2")
(etest (send i :foon :case1) "illegal use of :case method combination ~
       - :case not specified in the method")

;;; illegal
(defflavor a3 ()()(:method-combination (:case :base-flavor-first :foon)))
(defmethod (a3  :case :foon :case1) () () (format t "~% a fooon case1"))
(defmethod (a3  :foon :case2) () () (format t "~% a fooon case2"))
(test (setq i (make-instance 'a3)) "make-instance of a3")
(etest (send i :foon :case2) "illegal use of :case method combination - ~
       :case not specified in the method")

;;; illegal
(defflavor a5 ()())
(defmethod (a5 :case :foon :case1)()() (format t "~% a5 case1"))
(test (setq i (make-instance 'a5)) "make-instance of a5")
(etest (send i :foon :case1) "illegal use of :case method combination- ~
       :case not specified in flavor")

;; legal
(defflavor b8 () () (:method-combination (:case :base-flavor-first :foon)))
(defmethod (b8 :case :foon :caseb8) () ()
	   (format t "~% b8 foon caseb8"))
(defflavor a7 (b8) ()(:method-combination (:case :base-flavor-first :foon)))
(defmethod (a7 :case :foon :case1) () ()
	   (format t "~% a7 foon"))
(defmethod (a7 :case :foon :case2) () ()
	   (format t "~% a7 foon case2"))
(defmethod (a7 :before :foon) (&rest ignore) ()
 (format t "~% a7 :before :foon"))		
(defmethod (a7 :case :foon :otherwise) (&rest ignore) ()
	   (format t "~% a7 :case otherwise"))	
(test (setq i (make-instance 'a7)) "make-instance of a7")		
(test (test-stream-output " a7 :before :foon
 a7 :case otherwise"
			   (send i :foon :caseb8)
			   :print-stream *standard-output*)
      "test of :case combo with otherwise.  No method from~
      flavor components included") ;;;???
(test (test-stream-output " a7 :before :foon
 a7 foon"
			  (send i :foon :case1)
			  :print-stream *standard-output*)
      "test of :case combo  . :before method included")
(test (test-stream-output  " a7 :before :foon
 a7 foon case2"
			   (send i :foon :case2)
			   :print-stream *standard-output*)
      "test of :case combo with multiple keys specified by the same flavor")
(test (test-stream-output  " a7 :before :foon
 a7 :case otherwise"
			   (send i :foon 'otherwise)
			   :print-stream *standard-output*)
      "test of :case combo sending 'otherwise")

(test (test-stream-output   " a7 :before :foon
 a7 :case otherwise"
			   (send i :foon :goon)
			   :print-stream *standard-output*)
      "test of :case combo sending bogus message")


(test (setq i2 (make-instance 'b8)) "make-instance b8")
(test (test-stream-output " b8 foon caseb8" 
			  (send i2 :foon :caseb8)
			  :print-stream *standard-output*)
      "test of :case combo")   

;;; 
(defflavor b9 () () (:method-combination (:case :base-flavor-first :foon)))
(defmethod (b9 :case :foon :caseb9) () ()
	   (format t "~% b9 foon caseb9"))
(defflavor a9 (b9) ()(:method-combination (:case :base-flavor-first :foon)))
(defmethod (a9 :case :foon :case1) () ()
	   (format t "~% a9 foon"))
(test (setq i (make-instance 'a9)) "make-instance a9")
(etest (send i :foon :caseb9) "ettest- sending a :case message with unknown ~
       key errors as per Symbolics Rel 6")
(test (setq ib (make-instance 'b9)) "make-instance b9")
(test (test-stream-output " b9 foon caseb9" (send ib :foon :caseb9)
			  :print-stream *standard-output*)
      "test of :case method")

;;; :otherwise
(defflavor b10 () () (:method-combination (:case :base-flavor-first :foon)))
(defmethod (b10 :case :foon :caseb10) () ()
	   (format t "~% b10 foon caseb10"))
(defmethod (b10 :case :foon :otherwise) (message) ()	
	   (format t "~% b10 foon :otherwise"))
(test (setq i (make-instance 'b10)) "make-instance b10")
(test (test-stream-output " b10 foon :otherwise"
			   (send i :foon :foo)
			   :print-stream *standard-output*)
      "test of :otherwise clause in case combination");;;???

;;;
(defflavor bb3 () () (:method-combination (:case :base-flavor-first :foon)))
(defmethod (bb3  :case :foon :casebb3) () ()
	   (format t "~% bb3 foon casebb3"))
(defflavor aa3 (bb3) ())
(defmethod (aa3 :case :foon :case1) () ()
	   (format t "~% aa3 foon"))
(test (setq i (make-instance 'aa3)) "make-instance aa3")
(etest (send i :foon :casebb3)  "etest of :case combination")
(etest (send i :foon :case1) "etest of case combination") ;error

;;; from ICAD
(defflavor grocer () ()
	   (:method-combination (:case :base-flavor-last :buy-fruit)))

(defmethod (grocer :case :buy-fruit :otherwise ) (message quantity)
  (format t "Sorry, we have no ~a, not even ~d of them." message
	  quantity))

(defflavor ordinary ((bananas 100)
		     (oranges 100))
  (grocer))
(defmethod (ordinary :case :buy-fruit :bananas) (quantity)
  (decf bananas quantity))
(defmethod (ordinary :case :buy-fruit :oranges) (quantity)
  (decf oranges quantity))

(defflavor exotica ((papayas 100)
		    (mangos 100))
  (grocer))
(defmethod (exotica :case :buy-fruit :papayas) (quantity)
  (decf papayas quantity))
(defmethod (exotica :case :buy-fruit :mangos) (quantity)
  (decf mangos quantity))

(defflavor green-grocer ((cherries 100))
  (ordinary exotica grocer))
(defmethod (green-grocer :case :buy-fruit :cherries) (quantity)
  (decf cherries quantity))

(test (setq joe (make-instance 'green-grocer))
      "test make-instance green-grocer")
(test (eq 76(send joe :buy-fruit :cherries 24)) "test :case")
(test (eq 96(send joe :buy-fruit :mangos 4)) "test :case")
(test (test-stream-output "Sorry, we have no APPLES, not even 6 of them."
			  (send joe :buy-fruit :apples 6)
			  :print-stream *standard-output*)
      "test of :otherwise clause")

(defflavor az6 ()()(:method-combination (:case :base-flavor-first :foon)))
(defmethod (az6 :case :foon :case2) () () (format t "~% a fooon case2"))
(defmethod (az6 :case :foon :case1) () () (format t "~% a fooon case1"))
(defmethod (az6 :case :foon :case2) () () (format t "~% a fooon case2"))
(test (null (test-stream-output
	      "Found a duplicate key :CASE2 in CASE."
	      (make-instance 'az6)
	      :print-stream *error-output*))
      "test for bug 3189")
	  
	
