;;; Prime digit replacements
;; 
;; By replacing the 1st digit of the 2-digit number *3, it turns out
;; that six of the nine possible values: 13, 23, 43, 53, 73, and 83,
;; are all prime.
;; 
;; By replacing the 3rd and 4th digits of 56**3 with the same digit,
;; this 5-digit number is the first example having seven primes among
;; the ten generated numbers, yielding the family: 56003, 56113,
;; 56333, 56443, 56663, 56773, and 56993. Consequently 56003, being
;; the first member of this family, is the smallest prime with this
;; property.
;; 
;; Find the smallest prime which, by replacing part of the number (not
;; necessarily adjacent digits) with the same digit, is part of an
;; eight prime value family.

(in-package #:pe)

(defun eight-prime-family-p (s r)
  (loop for d across "0123456789"
     for n = (parse-integer (substitute-if d (lambda (x) (char= x r)) s))
     counting (and (p:primep n)
		   (> n 100000)) into c
     finally (return (= c 8))))

(defun euler-051 ()
  (loop with primes = (p:primes-upto 1000000)
     for p in primes
     for s = (prin1-to-string p)
     when (loop for x across "012"
	     when (= 3 (count-if (lambda (n) (char= n #\0)) s))
	     return (eight-prime-family-p s #\0))
     return p))
	   
