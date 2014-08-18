
;;; There exists exactly one Pythagorean triplet for which
;;; a + b + c = 1000.  Find the product abc.

(in-package #:pe)

(defun triple (n m)
  (let ((nsq (* n n))
	(msq (* m m)))
    (list (* 2 n m) (abs (- msq nsq)) (+ msq nsq))))

(defun triple-1 (n)
  (loop
     for m from 1 to 20
     for tr = (triple n m) then (triple n m)
     until (eql 1000 (apply #'+ tr))
     finally (return tr)))

(defun triplet ()
  (loop for n from 1 to 20
        for tr = (triple-1 n) then (triple-1 n)
        until (eql tr 1000)
        finally (return tr)))

(defun euler-009 ()
  (apply #'* (triplet)))

;(time (euler-009))
