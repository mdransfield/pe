;;;; Problem 95: Amicable Chains
;;;;
;;;; The proper divisors of a number are all the divisors excluding
;;;; the number itself. For example, the proper divisors of 28 are 1,
;;;; 2, 4, 7, and 14. As the sum of these divisors is equal to 28, we
;;;; call it a perfect number.
;;;;
;;;; Interestingly the sum of the proper divisors of 220 is 284 and
;;;; the sum of the proper divisors of 284 is 220, forming a chain of
;;;; two numbers. For this reason, 220 and 284 are called an amicable
;;;; pair.
;;;;
;;;; Perhaps less well known are longer chains. For example, starting
;;;; with 12496, we form a chain of five numbers:
;;;;
;;;; 12496 → 14288 → 15472 → 14536 → 14264 (→ 12496 → ...)
;;;;
;;;; Since this chain returns to its starting point, it is called an
;;;; amicable chain.
;;;;
;;;; Find the smallest member of the longest amicable chain with no
;;;; element exceeding one million.

(in-package #:pe)

(defparameter divisors-table (make-hash-table))

(defun get-divisors (n)
  "The proper divisors of N (1 ≤ K∣N < N)."
  (cons 1 (loop for k from 2 upto (1+ (isqrt n))
		for x = (multiple-value-list (floor n k))
		when (zerop (second x))
		  append (list k (first x)))))

(defun divisors (n cache)
  "The proper divisors of N (1 ≤ K∣N < N).
Attempts to look up divisors in CACHE in preference to computing them."
  (multiple-value-bind (val win) (gethash n cache)
    (if win
	val
	(setf (gethash n cache) (get-divisors n)))))

(defun sum-of-divisors-of (n cache)
  (apply #'+ (divisors n cache)))

(defun perfectp (n)
  "Predicate true when N is a perfect number ≤ 1,000,000."
  (or (= n 6) (= n 28) (= n 496) (= n 8128)))

(defun cycle (n cache)
  (cons n (loop for x = (sum-of-divisors-of n cache) then (sum-of-divisors-of x cache)
		for i from 1 below 30
		collect x
		until (or (= x 1) (= x n) (> x 1000000) (perfectp x)))))

(defun euler-095 ()
  (loop with cache = (make-hash-table)
	for i from 1 upto 1000000
	for c = (cycle i cache)
	when (= (first c) (first (last c)))
	  collect (cons i (1- (length c)))))
