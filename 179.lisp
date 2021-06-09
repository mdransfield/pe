;;; Problem 179: Consecutive positive divisors
;;
;; Find the number of integers 1<n<10⁷, for which n and n+1 have the
;; same number of positive divisors. For example, 14 has the positive
;; divisors 1, 2, 7, 14 while 15 has 1, 3, 5, 15.

(in-package #:pe)

(defun euler-179 (&optional (limit 10000000))
  (loop with sigma0 = (loop with s0 = (make-array (1+ limit) :initial-element 1)
			    for i from 2 upto limit
			    do (loop for f from 0 upto limit by i
				     do (incf (aref s0 f)))
			    finally (return s0))
	for i from 2 below limit
	count (= (aref sigma0 i) (aref sigma0 (1+ i)))))
