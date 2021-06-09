;;;; Problem 188: The hyperexponentiation of a number
;;
;; The hyperexponentiation or tetration of a number a by a positive
;; integer b, denoted by a↑↑b or ba, is recursively defined by:
;;
;; a↑↑1 = a,
;; a↑↑(k+1) = a(a↑↑k).
;;
;; Thus we have e.g. 3↑↑2=3³=27, hence 3↑↑3=3²⁷=7625597484987
;; and 3↑↑4 is roughly 10³.⁶³⁸³³⁴⁶⁴⁰⁰²⁴⁰⁹⁹⁶*¹⁰^¹².
;;
;; Find the last 8 digits of 1777↑↑1855.

(in-package #:pe)

(defun euler-188 ()
  (loop with modulus = (expt 10 8)
	for i from 2 upto 1855
	for x = (modular-pow 1777 1777 modulus)
	  then (modular-pow 1777 x modulus)
	finally (return x)))
