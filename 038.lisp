;;; Pandigital multiples
;;
;; Take the number 192 and multiply it by each of 1, 2, and 3:
;; 
;; 192 × 1 = 192
;; 192 × 2 = 384
;; 192 × 3 = 576
;; 
;; By concatenating each product we get the 1 to 9 pandigital,
;; 192384576. We will call 192384576 the concatenated product of 192
;; and (1,2,3)
;; 
;; The same can be achieved by starting with 9 and multiplying by 1,
;; 2, 3, 4, and 5, giving the pandigital, 918273645, which is the
;; concatenated product of 9 and (1,2,3,4,5).
;; 
;; What is the largest 1 to 9 pandigital 9-digit number that can be
;; formed as the concatenated product of an integer with (1,2,...,n)
;; where n > 1?

(in-package #:pe)

(defun pandigitalp (digits)
  (and (eql 9 (length digits))
       (equalp (sort (copy-seq digits) #'<) '#(1 2 3 4 5 6 7 8 9))))

(defun digits (n digits)
  (loop with temp = nil
        while (> n 0)
        do (multiple-value-bind (q r) (floor n 10)
	     (push r temp)
	     (setf n q))
        finally (loop for d in temp
		      do (vector-push-extend d digits)))
  digits)

(defun calc (n)
  (loop with num = (make-array 20 :element-type '(integer 0 9)
			       :adjustable t :fill-pointer 0)
        for i = 1 then (1+ i)
        do (digits (* n i) num)
        until (>= (length num) 9)
        finally (return num)))

(defun to-number (arr)
  (loop with n = 0
        for i across arr
        do (setf n (+ (* n 10) i))
        finally (return n)))

(defun euler-038 ()
  (loop for i from 2 to 99999
        for c = (calc i)
        when (pandigitalp c)
	  maximize (to-number c)))

;(time (euler-038))
