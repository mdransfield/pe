
;;; Find the sum of all the primes below two million.

(in-package #:pe)

(defun euler-010 ()
  (loop for i in (p:primes-upto 2000000)
        sum i))

;(time (euler-010))
