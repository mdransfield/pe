;;;; Problem 125: Palindromic sums

;; The palindromic number 595 is interesting because it can be written
;; as the sum of consecutive squares: 62+72+82+92+102+112+122.

;; There are exactly eleven palindromes below one-thousand that can be
;; written as consecutive square sums, and the sum of these
;; palindromes is 4164. Note that 1=0^2+1^2 has not been included as
;; this problem is concerned with the squares of positive integers.

;; Find the sum of all the numbers less than 10^8 that are both
;; palindromic and can be written as the sum of consecutive squares.

(in-package #:pe)

(defun euler-125 ()
  (loop with limit = (expt 10 8)
	with sqss = nil
	for i from 1 upto (sqrt limit)
	do (loop for j from (1+ i)
		 for sqs = (+ (* i i) (* j j)) then (+ sqs (* j j))
		 while (< sqs limit)
		 when (palindromep sqs)
		   do (pushnew sqs sqss))
	finally (return (apply #'+ sqss))))
