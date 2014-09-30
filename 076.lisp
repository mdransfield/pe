;;; Counting summations
;; It is possible to write five as a sum in exactly six different ways:
;;
;; 4 + 1
;; 3 + 2
;; 3 + 1 + 1
;; 2 + 2 + 1
;; 2 + 1 + 1 + 1
;; 1 + 1 + 1 + 1 + 1
;;
;; How many different ways can one hundred be written as a sum of at
;; least two positive integers?
 
(in-package #:pe)
 
(defun make-cache (coins max)
  (loop with cache = (make-hash-table)
        for c in coins
        do (setf (gethash c cache) (make-array (1+ max) :initial-element 0)
		 (aref (gethash c cache) 0) 1)
        finally (return cache)))
 
(defun ways (n coins cache)
  (let ((coin (car coins)))
    (cond
      ((null coin) 0)
      ((< n 0) 0)
      (t (let ((w (aref (gethash coin cache) n)))
	   (if (> w 0)
	       w
	       (setf (aref (gethash coin cache) n)
		     (+ (ways n (cdr coins) cache)
			(ways (- n coin) coins cache)))))))))
 
(defun euler-076 ()
  (let ((coins (loop for i from 1 upto 99 collect i)))
    (ways 100 coins (make-cache coins 100))))
 
;(time (euler-076))
