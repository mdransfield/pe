
;; Find the sum of all the even-valued terms in the Fibonacci sequence
;; which do not exceed four million.

(in-package #:pe)

(defun fib (limit)
  (labels ((ifib (acc limit)
	     (let* ((t1 (first acc))
		    (t2 (second acc))
		    (new-term (+ t1 t2)))
	       (if (<= new-term limit)
		   (ifib (append (list new-term) acc) limit)
		   acc))))
    (reverse (ifib (list 2 1) limit))))

(defun euler-002 ()
  (reduce #'+ (remove-if-not #'evenp (fibonacci:fibs-upto 4000000))))

;(euler-002)
