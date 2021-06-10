(in-package #:PE)

(defun phi (n)
  "Euler's Totient of N.
The number of integers less than N which are relatively prime to N."
  (* n (apply #'* (loop for (p . k) in (prime-factors-of n)
			collect (- 1 (/ 1 p))))))

(defun euler-069 (&optional (limit 1000000))
  (loop with imax = 0
	with xmax = 0
	for i from 2 below limit
	for x = (/ i (phi i))
	when (< xmax x)
	  do (setf xmax x
		   imax i)
	finally (return imax)))
