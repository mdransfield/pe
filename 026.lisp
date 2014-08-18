
;;; Find the value of d < 1000 for which ^(1)/_(d) contains the
;;; longest recurring cycle in its decimal fraction part.

(in-package #:pe)

(defun cycle-length (n)
  (let ((count 1)
	(divisors (list n)))
    (loop
       (multiple-value-bind (q r) (floor (* 10 n))
	 (declare (ignorable q))
	 (if (zerop r)
             (return 0)
           (if (member r divisors)
               (return count)
	     (progn
	       (push r divisors)
	       (incf count)
	       (setf n r))))))))

(defun euler-026 ()
  (loop with max-n = 0
        with max-l = 0
        for n from 2 below 1000
        for l = (cycle-length (/ 1 n)) then (cycle-length (/ 1 n))
        when (>  l max-l)
        do (setf max-l l
		 max-n n)
        finally (return (values max-n max-l))))

;(time (euler-026))
