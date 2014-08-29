;;; Pandigital primes
;;
;; We shall say that an n-digit number is pandigital if it makes use
;; of all the digits 1 to n exactly once. For example, 2143 is a
;; 4-digit pandigital and is also prime.
;;
;; What is the largest n-digit pandigital prime that exists?

(in-package #:pe)

(defun digits (n)
  (loop with temp = nil
        while (> n 0)
        do (multiple-value-bind (q r) (floor n 10)
	     (push r temp)
	     (setf n q))
        finally (return temp)))

(defun pandigitalp (n)
  (equal (sort (digits n) #'<)
	 (loop for i from 1 to (ceiling (log n 10))
	       collect i)))

(defun euler-041 ()
  (loop for i from 7654321 downto 1234567 by 2
        when (and (pandigitalp i) (primep i)) return i))
