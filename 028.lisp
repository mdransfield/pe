
;;  Starting with the number 1 and moving to the right in a clockwise direction a 5 by 5 spiral is formed as follows:
;;  
;;  21 22 23 24 25
;;  20  7  8  9 10
;;  19  6  1  2 11
;;  18  5  4  3 12
;;  17 16 15 14 13
;;  
;;  It can be verified that the sum of both diagonals is 101.
;;
;;; What is the sum of both diagonals in a 1001 by 1001 spiral formed
;;; in the same way?

(in-package #:pe)

(defun sum-series (f base limit)
  (loop for i from base to limit
        sum (funcall f i)))

(defun euler-028 ()
  (+ (sum-series (lambda (n) (expt (1+ (* n 2)) 2)) 0 500)
     (sum-series (lambda (n) (1+ (* 4 n n))) 0 500)
     (sum-series (lambda (n) (+ 1 (- n) (* n n))) 1 1001)
     -2))

;(time (euler-028))
