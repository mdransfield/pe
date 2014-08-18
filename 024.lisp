
;;; What is the millionth lexicographic permutation of the digits 0,
;;; 1, 2, 3, 4, 5, 6, 7, 8 and 9?

(in-package #:pe)

(defun factorial (n)
  (labels ((ifact (n x)
	     (if (zerop n) x
		 (ifact (1- n) (* n x)))))
    (ifact n 1)))

(defun get-tempi (k f l i)
   (mod (floor k f) (- l i)))

(defun permutation (k s)
  (loop with l = (length s)
        with fact = (factorial (1- l))
        for i from 0 below (1- l)
        for tempi = (get-tempi k fact l i) then (get-tempi k fact l i)
        for temp = (aref s (+ i tempi)) then (aref s (+ i tempi))
        do (loop for j = (+ i tempi) then (1- j)
		 while (> j i)
		 do (setf (aref s j) (aref s (1- j))))
           (setf (aref s i) temp)
           (setf fact (floor fact (- l i 1))))
  s)

(defun euler-024 ()
  (permutation 999999 (make-array 10 :initial-contents '(0 1 2 3 4 5 6 7 8 9))))

;(time (euler-024))
