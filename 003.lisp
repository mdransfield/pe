
;; What is the largest prime factor of the number 600851475143 ?

(in-package #:pe)

(defun euler-003 ()
  (loop for p in (p:primes-upto (ceiling (sqrt 600851475143)))
        when (eql 0 (mod 600851475143 p)) maximize p))

;(euler-003)