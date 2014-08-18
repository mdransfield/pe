
;;; What is the sum of the digits of the number 2^(1000)?

(in-package #:pe)

(defun euler-016 ()
  (loop for n = (expt 2 1000) then (floor (/ n 10))
        for d = (mod n 10) then (mod n 10)
        with s = 0
        while (> n 0) do (incf s d)
        finally (return s)))