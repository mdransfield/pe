;;;; Problem 119: Digit power sum
;;
;; The number 512 is interesting because it is equal to the sum of its
;; digits raised to some power: 5+1+2=8, and 8³=512. Another
;; example of a number with this property is 614656 = 28⁴.
;;
;; We shall define aₙ to be the nth term of this sequence and insist
;; that a number must contain at least two digits to have a sum.
;;
;; You are given that a₂=512 and a₁₀=614656.
;;
;; Find a₃₀.

(in-package #:PE)

(defun euler-119 (&optional (limit 30))
  (loop with solutions = nil
	with nsol = 0
	for e from 2 upto 1000
	do (loop for b from 2 upto 1000
		 for p = (expt b e)
		 when (= b (apply #'+ (digits-of p)))
		   do (incf nsol)
		      (push p solutions)
		 until (>= nsol limit))
	finally (return (nth (1- limit) (sort solutions #'<)))))
