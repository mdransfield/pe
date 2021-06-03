;;;; Problem 85: Counting rectangles
;;
;; By counting carefully, it can be seen that a rectangular grid
;; measuring 3 by 3 contains eighteen rectangles:
;;
;;  6      +   4      +   2      +
;; ._. . .    ._._. .    ._._._.  
;; |_| . .    |_._| .    |_._._|  
;; . . . .    . . . .    . . . .  
;;
;;  3      +   2      +   1	 =  18
;; ._. . .    ._._. .    ._._._.
;; | | . .    | . | .    | . . |
;; |_| . .    |_._| .    |_._._|
;;
;; Although there exists no rectangular grid that contains exactly two
;; million rectangles, find the area of the grid with the nearest
;; solution.

(in-package #:PE)

(defun euler-085 ()
  (loop with limit = 2000000
	with distance = limit
	with nearest = 0
	with na = 0
	with nb = 0
	for a from 1 upto 100
	do (loop for b from 1 upto 100
		 for nrec = (* 1/4 a (1+ a) b (1+ b))
		 for dist = (abs (- 2000000 nrec))
		 when (< dist distance)
		   do (setf nearest nrec
			    na a
			    nb b
			    distance (- 2000000 nrec)))
	finally (return (* na nb))))
