
;;; Find the sum of all numbers which are equal to the sum of the
;;; factorial of their digits.

(in-package #:pe)

(defun calc-digits (n)
  (loop with digits = nil
        while (> n 0)
        do (multiple-value-bind (q r) (floor n 10)
	     (push r digits)
	     (setf n q))
        finally (return digits)))

(defparameter digits-table (make-hash-table))

(defun digits (n)
  (multiple-value-bind (val win) (gethash n digits-table)
    (if win
	val
	(setf (gethash n digits-table) (calc-digits n)))))

(defun wantedp (n)
  (eql n (reduce #'+ (mapcar #'factorial (digits n)))))

(defun euler-034 ()
  (reduce #'+ (loop for n from 3 to (factorial 10)
		    when (wantedp n)
		    collect n)))

;(time (euler-034))

