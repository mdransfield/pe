;;; There are exactly ten ways of selecting three from five, 12345:
;;;
;;; 123, 124, 125, 134, 135, 145, 234, 235, 245, and 345
;;;
;;; In combinatorics, we use the notation, ^(5)C_(3) = 10.
;;;
;;; In general,
;;;
;;;                n!
;;; ^(n)C_(r) = --------, where r ≤ n, n! = n×(n−1)×...×3×2×1,
;;;             r!(n−r)!    and 0! = 1.
;;;
;;; It is not until n = 23, that a value exceeds one-million:
;;; ^(23)C_(10) = 1144066.
;;;
;;; How many, not necessarily distinct, values of ^(n)C_(r), for 1 ≤
;;; n ≤ 100, are greater than one-million?

(in-package #:pe)

(defun c (n r)
  (/ (factorial n) (* (factorial r) (factorial (- n r)))))

(defun euler-053 ()
  (loop for n from 1 upto 100
        with count = 0
        do (loop for r from 1 upto n
		 when (> (c n r) 1000000)
		 do (incf count))
        finally (return count)))

;;(time
;;(euler-053)
;;)
