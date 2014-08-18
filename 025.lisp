
;;; What is the first term in the Fibonacci sequence to contain 1000 digits?

(in-package #:pe)

(defparameter fib-count 2)
(defparameter fib-n1 1)
(defparameter fib-n2 1)

(defun reset-fib ()
  (setf fib-count 2
	fib-n2 1
	fib-n1 1))

(defun next-fib ()
  (let ((n (+ fib-n1 fib-n2)))
    (setf fib-n1 fib-n2
	  fib-n2 n
	  fib-count (1+ fib-count))
    (values fib-count n)))

(defun num-digits (n)
  (1+ (floor (log n 10))))

(defun euler-025 ()
  (labels ((e25 (n)
	     (reset-fib)
	     (loop
	       (multiple-value-bind (c v) (next-fib)
		 (when (eql n (num-digits v))
		   (return c))))))
    (e25 1000)))

; (time (euler-025 1000))

