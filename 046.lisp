
;;; Goldbach's other conjecture
;;
;; It was proposed by Christian Goldbach that every odd composite
;; number can be written as the sum of a prime and twice a square.
;; 
;; It turns out that the conjecture was false.
;;
;; What is the smallest odd composite that cannot be written as the
;; sum of a prime and twice a square?

(in-package #:pe)

(defparameter primes (p::make-sieve 1000000))

(defun next-prime (n)
  (loop for i = (1+ n) then (1+ i)
        if (> i limit)
	  do (error "Out of primes")
        until (= 1 (bit primes i))
        finally (return i)))

(defun conjectural-p (n)
  (loop with i = 1
        with temp
        with p = 2
        while (< p n) do
	  (setf temp (+ p (* 2 i i)))
        if (= temp n) return t
	  if (> temp n) do
	    (setf p (next-prime p)
		  i 1)
        else do (incf i)
        finally (return nil)))

(defun euler-046 ()
  (loop for i = 35 then (+ i 2)
        if (and (not (p:primep i))
		(not (conjectural-p i)))
        return i))

;(time (euler-046))

