;;;; Problem 120: Square remainders
;;
;; Let r be the remainder when (a−1)^n + (a+1)^n is divided by a^2.
;;
;; For example, if a = 7 and n = 3, then r = 42: 63 + 83 = 728 ≡ 42
;; mod 49. And as n varies, so too will r, but for a = 7 it turns out
;; that r_max = 42.
;;
;; For 3≤a≤1000, find ∑ r_max.

(in-package #:pe)

(defun euler-120 ()
  (labels ((rmax (a) (* 2 a (floor (1- a) 2))))
    (loop for a from 3 upto 1000
	  sum (rmax a))))
