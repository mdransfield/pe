
;;; Find the sum of all the numbers that can be written as the sum of
;;; fifth powers of their digits.

(in-package #:pe)

(defun digits (n)
  (loop with digits = nil
        while (> n 0)
        do (multiple-value-bind (q r) (floor n 10)
	     (push r digits)
	     (setf n q))
        finally (return digits)))

(defun euler-030 ()
  (loop for i from 1 to (* 6 (expt 9 5))
        when (and (not (eql 1 i))
		  (eql i (reduce #'+
				 (mapcar (lambda (x) (expt x 5))
					 (digits i)))))
        sum i))

;(time (euler-030))
