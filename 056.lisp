;;; A googol (10^(100)) is a massive number: one followed by
;;; one-hundred zeros; 100^(100) is almost unimaginably large: one
;;; followed by two-hundred zeros. Despite their size, the sum of the
;;; digits in each number is only 1.
;;;
;;; Considering natural numbers of the form, a^(b), where a, b < 100,
;;; what is the maximum digital sum?

(in-package #:pe)

(defun digits (n)
  (loop with temp = nil
        while (> n 0)
        do (multiple-value-bind (q r) (floor n 10)
	     (push r temp)
	     (setf n q))
        finally (return temp)))

(defun euler-056 ()
  (loop
     for a from 1 upto 100
     maximizing (loop
		   for b from 1 upto 100
		   maximizing (apply #'+ (digits (expt a b))))))

;(time (euler-056))
