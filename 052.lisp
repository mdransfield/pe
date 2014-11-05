;;; It can be seen that the number, 125874, and its double, 251748,
;;; contain exactly the same digits, but in a different order.
;;;
;;; Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x,
;;; and 6x, contain the same digits.

(in-package #:pe)

(defun digits (n)
  (sort (loop with digits = nil
	   while (> n 0)
	   do (multiple-value-bind (q r) (floor n 10)
		(push r digits)
		(setf n q))
	   finally (return digits)) #'<))

(defun euler-052 ()
  (loop
     for n = 1 then (1+ n)
     for d1 = (digits n)
     for d2 = (digits (* 2 n))
     for d3 = (digits (* 3 n))
     for d4 = (digits (* 4 n))
     for d5 = (digits (* 5 n))
     for d6 = (digits (* 6 n))
       when (and (equal d1 d2)
		 (equal d3 d4)
		 (equal d5 d6)
		 (equal d1 d3)
		 (equal d1 d5)) return n))

;(time (euler-052))
