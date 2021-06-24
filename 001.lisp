
;; Add all the natural numbers below one thousand that are multiples
;; of 3 or 5.

(in-package #:pe)

(defun euler-001 (&optional (limit 1000))
  (labels ((part-sum (n)
	     (let ((num (floor (1- limit) n)))
	       (* 1/2 num (1+ num) n))))
    (+ (part-sum 3) (part-sum 5) (part-sum -15))))
