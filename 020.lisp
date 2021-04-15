
;;; Find the sum of the digits in the number 100!

(in-package #:pe)

(defun sum-digits (n)
  (loop with s = 0
        while (> n 0)
        do (incf s (mod n 10))
           (setf n (floor n 10))
        finally (return s)))

(defun euler-020 ()
  (sum-digits (factorial 100)))
