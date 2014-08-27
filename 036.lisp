;;; Double-base palindromes
;; 
;; The decimal number 585 = 1001001001 (binary) is palindromic in both
;; bases.
;;
;; Find the sum of all numbers, less than one million, which are
;; palindromic in base 10 and base 2.

(in-package #:pe)

(defun nums (n base vec)
  (setf (fill-pointer vec) 0)
  (loop while (> n 0)
        do (multiple-value-bind (q r) (floor n base)
	     (vector-push-extend r vec)
	     (setf n q))
        finally (return vec)))

(defun bits (n bits)
  (nums n 2 bits))
        
(defun digits (n digits)
  (nums n 10 digits))

(defun palindromep (n)
  (equalp n (reverse n)))

(defun euler-036 ()
  (loop with dvec = (make-array 10
				:element-type '(integer 0 9)
				:adjustable t
				:fill-pointer 0)
        with bvec = (make-array 20
				:element-type 'bit
				:adjustable t
				:fill-pointer 0)
        for i from 1 below 1000000
        when (and (palindromep (bits i bvec))
		  (palindromep (digits i dvec)))
        sum i))

;(time (euler-036))
