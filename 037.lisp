;;; Truncatable primes
;; 
;; The number 3797 has an interesting property. Being prime itself, it
;; is possible to continuously remove digits from left to right, and
;; remain prime at each stage: 3797, 797, 97, and 7. Similarly we can
;; work from right to left: 3797, 379, 37, and 3.
;;
;; Find the sum of the only eleven primes that are both truncatable
;; from left to right and right to left.
;; 
;; NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.

(in-package #:pe)

(defun left-truncatable-p (p)
  (loop for power from (floor (log p 10)) downto 1
	for x = (mod p (expt 10 power))
	unless (p:primep x)
	  return nil
	finally (return t)))

(defun right-truncatable-p (p)
  (loop for x = (floor p 10) then (floor x 10)
	while (> x 0)
	unless (p:primep x)
	  return nil
	finally (return t)))

(defun euler-037 ()
  (loop for p in (p:primes-upto 1000000)
	when (and (> p 10)
		  (left-truncatable-p p)
		  (right-truncatable-p p))
	  sum p))
