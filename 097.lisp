;;;; Problem 97: Non-Mersenne Prime
;;
;; The first known prime found to exceed one million digits was
;; discovered in 1999, and is a Mersenne prime of the form 26972593−1;
;; it contains exactly 2,098,960 digits. Subsequently other Mersenne
;; primes, of the form 2p−1, have been found which contain more
;; digits.
;;
;; However, in 2004 there was found a massive non-Mersenne prime which
;; contains 2,357,207 digits: 28433×27830457+1.
;;
;; Find the last ten digits of this prime number.

(in-package :PE)

;; Algorithm for modular exponentation, from Schneier B., 1996,
;; Applied Cryptography: Protocols, Algorithms and Source Code in C.
;;
;; function modular_pow(base, exponent, modulus) is
;;     if modulus = 1 then
;;         return 0
;;     Assert :: (modulus - 1) * (modulus - 1) does not overflow base
;;     result := 1
;;     base := base mod modulus
;;     while exponent > 0 do
;;         if (exponent mod 2 == 1) then
;;             result := (result * base) mod modulus
;;         exponent := exponent >> 1
;;         base := (base * base) mod modulus
;;     return result

(defun modular-pow (base exponent modulus)
  "Calculate (mod (expt BASE EXPONENT) MODULUS) efficiently."
  (if (= 1 modulus)
      0
      (loop with result = 1
	    with base = (mod base modulus)
	    while (> exponent 0)
	    if (= (mod exponent 2) 1)
	      do (setf result (mod (* result base) modulus))
	    do (setf exponent (ash exponent -1)
		     base (mod (* base base) modulus))
	       finally (return result))))

(defun euler-097 ()
  (let ((modulus 10000000000))
    (mod (1+ (* 28433 (modular-pow 2 7830457 modulus))) modulus)))
