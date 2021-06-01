;;;; Problem 78: Coin Partitions
;;
;; Let p(n) represent the number of different ways in which n coins
;; can be separated into piles. For example, five coins can be
;; separated into piles in exactly seven different ways, so p(5)=7.
;;
;; OOOOO
;; OOOO   O
;; OOO   OO
;; OOO   O   O
;; OO   OO   O
;; OO   O   O   O
;; O   O   O   O   O
;;
;; Find the least value of n for which p(n) is divisible by one million.

(in-package #:pe)

(defun euler-078 ()
  (loop with p = (make-array 100000 :initial-element 0)
	with d = 1000000
	for n from 0
	do (setf (aref p 0) 1
		 (aref p 1) 1)
	   (loop for k from 1
		 for x = (- n (* 1/2 k (1- (* 3 k))))
		 for y = (- n (* 1/2 k (1+ (* 3 k))))
		 for s = 1 then (* -1 s)
		 when (>= x 0)
		   do (incf (aref p n) (* s (aref p x)))
		 when (>= y 0)
		   do (incf (aref p n) (* s (aref p y)))
		 until (and (< x 0) (< y 0)))
	   (setf (aref p n) (mod (aref p n) d))
	until (zerop (mod (aref p n) d))
	finally (return n)))
