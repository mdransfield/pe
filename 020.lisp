
;;; Find the sum of the digits in the number 100!

(in-package #:pe)

(defun factorial (n)
  (labels ((ifact (n x)
             (if (zerop n)
                 x
               (ifact (1- n) (* n x)))))
    (ifact n 1)))

(defun sum-digits (n)
  (loop with s = 0
        while (> n 0)
        do (incf s (mod n 10))
           (setf n (floor n 10))
        finally (return s)))

(defun euler-020 ()
  (sum-digits (factorial 100)))