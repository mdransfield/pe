;;; Shortest distance among points
;;
;; We create an array of points Pn in a two dimensional plane using
;; the following random number generator:
;;
;; s(0) = 290797
;; s(n+1) = s(n)² mod 50515093
;;
;; P(n) = (s(2n),s(2n+1))
;;
;; Let d(k) be the shortest distance of any two (distinct) points
;; among P(0), ..., P(k−1).
;;
;; E.g. d(14) = 546446.466846479
;;
;; Find d(2000000).  Give your answer rounded to 9 places after the
;; decimal point.

(in-package #:pe)

(defun next-s (s)
  (if (zerop s)
      290797
      (modular-pow s 2 50515093)))

(defun make-p (n)
  (loop with p = (make-array n :element-type 'complex)
	for i from 0 below n
	for s = (next-s 0) then (next-s (next-s s))
	do (setf (aref p i) (complex s (next-s s)))
	finally (return (sort p #'< :key #'realpart))))

(defun dist (p1 p2)
  (let ((x1 (realpart p1))
	(y1 (imagpart p1))
	(x2 (realpart p2))
	(y2 (imagpart p2)))
    (sqrt (+ (expt (- x2 x1) 2d0) (expt (- y2 y1) 2d0)))))


(defun find-min (p i j)
  (case (abs (- i j))
    (1 (dist (aref p i) (aref p j)))
    (2 (min (dist (aref p i) (aref p (1+ i))) (dist (aref p (1+ i)) (aref p j)) (dist (aref p i) (aref p j))))
    (otherwise
     (let ((m (floor (abs (- i j)) 2)))
       (min (find-min p i (+ i m)) (find-min p (+ i m) j))))))

(defun euler-816 ()
  (let* ((k 2000000)
	 (p (make-p k)))
    (find-min p 0 (1- k))))
