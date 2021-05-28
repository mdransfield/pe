;;;; Problem 123: Prime square remainders
;;
;; Let p_n be the nth prime: 2, 3, 5, 7, 11, ..., and let r be the
;; remainder when (p_n −1)^n + (p_n + 1)^n is divided by p_n^2.
;;
;; For example, when n = 3, p_3 = 5, and 43 + 63 = 280 ≡ 5 mod 25.
;;
;; The least value of n for which the remainder first exceeds 10^9 is
;; 7037.
;;
;; Find the least value of n for which the remainder first exceeds
;; 10^10.

(in-package :pe)

(defun euler-123 ()
  (loop with p = (apply #'vector 1 (p:primes-upto 1000000))
	for n from 7038 below (length p) by 2
	when (> (* 2 (1+ n) (aref p n)) (expt 10 10))
	  return (1+ n)))
