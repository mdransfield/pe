;;;; Problem 87: Prime power triples
;;
;; The smallest number expressible as the sum of a prime square, prime
;; cube, and prime fourth power is 28. In fact, there are exactly four
;; numbers below fifty that can be expressed in such a way:
;;
;; 28 = 2² + 2³ + 2⁴
;; 33 = 3² + 2³ + 2⁴
;; 49 = 5² + 2³ + 2⁴
;; 47 = 2² + 3³ + 2⁴
;;
;; How many numbers below fifty million can be expressed as the sum of
;; a prime square, prime cube, and prime fourth power?

(in-package #:PE)

(defun euler-087 (&optional (limit 50000000))
  (loop with primes = (p:primes-upto (ceiling (sqrt limit)))
	with solutions = (make-array limit :element-type 'bit)
	for i in primes
	do (loop for j in primes
		 for s0 = (+ (expt j 3) (expt i 2))
		 when (< s0 limit)
		   do (loop for k in primes
			  for s = (+ (expt k 4) s0)
			  when (< s limit)
			    do (setf (bit solutions s) 1)))
	   finally (return (count 1 solutions))))
