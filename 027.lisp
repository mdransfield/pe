
;;; Find the product of the coefficients, a and b, for the quadratic
;;; expression that produces the maximum number of primes for
;;; consecutive values of n, starting with n = 0.

(in-package #:pe)

(defun consecutive-primes (a b)
  (loop for n = 0 then (1+ n)
        while (p:primep (+ (* n n) (* a n) b)) count n))

(defun euler-027 ()
  (loop with max-primes = 0
        with max-prod = 0
        for a from -1000 to 1000
        do (loop for b from -1000 to 1000
		 for num-primes = (consecutive-primes a b) then (consecutive-primes a b)
		 if (> num-primes max-primes)
		 do (setf max-primes num-primes
			  max-prod (* a b)))
        finally (return (values max-primes max-prod))))

;(time (euler-027))
