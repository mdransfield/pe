
;;; Find the sum of all the positive integers which cannot be written
;;; as the sum of two abundant numbers.
;;
;;  A perfect number is a number for which the sum of its proper
;;  divisors is exactly equal to the number. For example, the sum of
;;  the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which
;;  means that 28 is a perfect number.
;;
;;  A number whose proper divisors are less than the number is called
;;  deficient and a number whose proper divisors exceed the number is
;;  called abundant.
;;
;;  As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the
;;  smallest number that can be written as the sum of two abundant
;;  numbers is 24. By mathematical analysis, it can be shown that all
;;  integers greater than 28123 can be written as the sum of two
;;  abundant numbers. However, this upper limit cannot be reduced any
;;  further by analysis even though it is known that the greatest
;;  number that cannot be expressed as the sum of two abundant numbers
;;  is less than this limit.

(in-package #:pe)

(defun divisors (n)
  (loop with divisors = (list 1 n)
        for i from 2 to (floor (sqrt n))
        do (multiple-value-bind (q r) (floor n i)
	     (when (zerop r)
	       (pushnew i divisors)
	       (pushnew q divisors)))
        finally (return divisors)))

(defvar *sum-of-divisors-table* (make-hash-table))

(defun sum-of-divisors (n)
  (let ((lookup (gethash n *sum-of-divisors-table*)))
    (unless lookup
      (setf lookup (setf (gethash n *sum-of-divisors-table*)
			 (reduce #'+ (divisors n)))))
    lookup))

(defun abundantp (n)
  (> (sum-of-divisors n) (* 2 n)))

(defvar *abundant-numbers*
  (loop for i from 1 to 28123
	when (abundantp i) collect i))

(defun sum-of-two-abundant-numbers-p (n)
  (if (or (>= n 945) (evenp n))
      (loop for tn in *abundant-numbers*
            for d = (- n tn) then (- n tn)
            while (< tn n)
            when (abundantp d) return (cons tn d)
	    finally (return nil))))

(defun numbers ()
  (loop for i from 1 to 28123
        collect i))

(defun euler-023 ()
  (reduce #'+ (remove-if #'sum-of-two-abundant-numbers-p (numbers))))


;(time (euler-023))
