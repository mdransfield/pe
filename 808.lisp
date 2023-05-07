;;; Reversible prime squares
;;
;; Both 169 and 961 are the square of a prime. 169 is the reverse of
;; 961.
;;
;; We call a number a reversible prime square if:
;;
;; 1. It is not a palindrome, and
;; 2. It is the square of a prime, and
;; 3. Its reverse is also the square of a prime.
;;
;; 169 and 961 are not palindromes, so both are reversible prime
;; squares.
;;
;; Find the sum of the first 50 reversible prime squares.

(in-package #:PE)

(defun euler-808 ()
  (loop with prime-squares = (loop with h = (make-hash-table :size 100000)
				   for n in (p:primes-upto 40000000)
				   for p = (expt n 2)
				   for r = (reversed p)
				   when (and (or (= 1 (mod r 10))
						 (= 9 (mod r 10)))
					     (/= p r))
				     do (setf (gethash p h) t)
				   finally (return h))
	for i being each hash-key of prime-squares
	when (gethash (reversed i) prime-squares)
	  sum i))
