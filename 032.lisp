
;;; Find the sum of all products whose multiplicand/multiplier/product
;;; identity can be written as a 1 through 9 pandigital.

(in-package #:pe)

(defun pandigitalp (digits)
  (and (eql 9 (length digits))
       (equal (sort (copy-seq digits) #'<) '(1 2 3 4 5 6 7 8 9))))

(defun calc-digits (n)
  (loop with digits = nil
        while (> n 0)
        do (multiple-value-bind (q r) (floor n 10)
	     (push r digits)
	     (setf n q))
        finally (return digits)))

(defparameter digits-table (make-hash-table))

(defun digits (n)
  (let ((d (gethash n digits-table)))
      (unless d
	(setf (gethash n digits-table) (calc-digits n))
	(setf d (gethash n digits-table)))
      d))

(defun products (n)
  (loop with products = nil
        for i from 1 to n
        do (loop for j from 1 to n
		 for p = (* i j) then (* i j)
		 when (pandigitalp (append (digits i) (digits j) (digits p)))
		 do (pushnew p products))
        finally (return products)))

(defun euler-032 ()
  (reduce #'+ (products 2000)))

;(time (euler-032))

