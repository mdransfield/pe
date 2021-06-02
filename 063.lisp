;;;; Problem 63: Powerful digit counts
;;
;; The 5-digit number, 16807=7^5, is also a fifth power. Similarly,
;; the 9-digit number, 134217728=8^9, is a ninth power.
;;
;; How many n-digit positive integers exist which are also an nth
;; power?

(in-package #:PE)

(defun euler-063 ()
  (loop for n from 1 below 22
	sum (loop for x from 1 below 10
		  for p = (expt x n)
		  count (= n (1+ (floor (log p 10)))))))
