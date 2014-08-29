
;;; Champernowne's constant
;;
;; An irrational decimal fraction is created by concatenating the
;; positive integers:
;;
;;     0.123456789[1]01112131415161718192021...
;;
;; It can be seen that the 12th digit of the fractional part is 1.
;; 
;; If d_n represents the nth digit of the fractional part, find the
;; value of the following expression.
;; 
;; d_1 × d1_0 × d_100 × d_1000 × d_10000 × d_100000 × d_1000000

(in-package #:pe)

(defun digits (n)
  (loop with digits = nil
        while (> n 0)
        do (multiple-value-bind (q r) (floor n 10)
	     (push r digits)
	     (setf n q))
        finally (return digits)))

(defun euler-040 ()
  (loop with count = 1
        with product = 1
        for n = 2 then (1+ n)
	for x = (digits n)
        do (loop for d in (digits n)
                 do (incf count)
                 when (= (log count 10) (floor (log count 10)))
                 do (setf product (* product d)))
        until (> count 1000000)
        finally (return product)))
