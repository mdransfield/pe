
;;; How many routes are there through a 20×20 grid?

(in-package #:pe)

(defun factorial (n)
  (labels ((ifact (n x)
	     (if (eql n 1)
		 x
		 (ifact (1- n) (* n x)))))
    (ifact n 1)))

(defun binomial-coefficient (n k)
  (/ (factorial n)
     (* (factorial k) (factorial (- n k)))))

(defun binomial-coefficients (n)
  (loop for k from 1 below n
        do (format t "~&~d~%" (binomial-coefficient n k))))

(defun euler-015 ()
  (binomial-coefficient 40 20))

;(time (euler-015))
