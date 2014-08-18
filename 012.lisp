
;;; What is the value of the first triangle number to have over five
;;; hundred divisors?

(in-package #:pe)

(defparameter *triangle* 1)

(defvar *next-triangle* 2)

(let ((triangle 1)
      (next-triangle 2))
  (defun next-triangle ()
    (incf triangle next-triangle)
    (incf next-triangle)
    triangle)
  (defun reset-triangle ()
    (setf triangle 1
	  next-triangle 2)))

(defun divisors (n)
  (loop with divisors = (list 1 n)
        for i from 2 to (floor (sqrt n))
        do (multiple-value-bind (q r) (floor n i)
	     (when (zerop r)
	       (pushnew i divisors)
	       (pushnew q divisors)))
        finally (return divisors)))

(defun euler-012 ()
  (loop for tr = (next-triangle) then (next-triangle)
        until (> (length (divisors tr)) 500)
        finally (return tr)))