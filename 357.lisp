;;;; Problem 357: Prime generating integers
;;
;; Consider the divisors of 30: 1,2,3,5,6,10,15,30.
;; It can be seen that for every divisor d of 30, d+30/d is prime.
;;
;; Find the sum of all positive integers n not exceeding 100,000,000
;; such that for every divisor d of n, d+n/d is prime.

(in-package #:pe)

(defparameter limit (expt 10 8))
(defparameter primes nil)

(defun trial (d n)
  (+ d (/ n d)))

(defun primep (n)
  (unless primes
    (setf primes (p:make-sieve (1+ limit))))
  (p:sieve-prime-p n primes))
 
(defun wantedp (n)
  (loop for i from 1 upto (sqrt n)
	for f = (multiple-value-list (floor n i))
	when (zerop (second f))
	  do (unless (primep (trial (first f) n))
	       (return 0))
	finally (return n)))

(defun euler-357 ()
  (1+ (loop for n from 2 upto limit by 4
	    when (primep (1+ n))
	      sum (wantedp n))))

