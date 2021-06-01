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

(defvar *partition-cache* (make-hash-table))

(defun partition-count (n)
  (cond
    ((< n 0) 0)
    ((= 0 n) 1)
    (t
     (multiple-value-bind (val win) (gethash n *partition-cache*)
       (if win
	   val
	   (setf (gethash n *partition-cache*) (calculate-partition-count n)))))))

(defun calculate-partition-count (n)
  (loop for k from 1 upto n
	sum (* (expt -1 (1+ k)) (+ (partition-count (- n (* 1/2 k (1- (* 3 k))))) (partition-count (- n (* 1/2 k (1+ (* 3 k)))))))))

(defun euler-078 ()
  (loop for i from 0
	when (zerop (mod (partition-count i) 1000000))
	  return i))
