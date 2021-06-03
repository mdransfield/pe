;;;; Problem 64: Odd period square roots
;;
;; All square roots are periodic when written as continued fractions
;; and can be written in the form: √N = a_0 + 1 / (a_1 + 1/ (a_2 + 1/ (a_3 + ...)))
;;
;; How many continued fractions for N ≤ 10,000 have an odd period?

(in-package :PE)

(defun euler-064 (&optional (x 2) (y 10000) (p 1024))
  (loop with a = (make-array 1001)
	for n from x upto y
	unless (= (expt (isqrt n) 2) n)
	  count (oddp (multiple-value-bind (q0 r0) (floor (cr:rational-approx-r (cr:sqrt-r n) p))
			(setf (aref a 0) q0)
			(loop for i from 1
			      with r = r0
			      do (multiple-value-bind (qi ri) (floor (/ r))
				   (setf (aref a i) qi
					 r ri))
			      until (= (* 2 (aref a 0)) (aref a i))
			      finally (return i))))))
