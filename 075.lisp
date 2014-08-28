
;;; Singular integer right triangles
;; It turns out that 12cm is the smallest length of wire that can be
;; bent to form an integer sided right angle triangle in exactly one
;; way, but there are many more examples.
;;
;; 12cm: (3,4,5)
;; 24cm: (6,8,10)
;; 30cm: (5,12,13)
;; ...
;;
;; In contrast, some lengths of wire, like 20 cm, cannot be bent to
;; form an integer sided right angle triangle, and other lengths allow
;; more than one solution to be found; for example, using 120 cm it is
;; possible to form exactly three different integer sided right angle
;; triangles.
;;
;; Given that L is the length of the wire, for how many values of L d
;; 1,500,000 can exactly one integer sided right angle triangle be
;; formed?

(in-package #:pe)

(defun euler-075 ()
  (loop with l = 1500000
	with counts = (make-array (1+ l) :initial-element 0)
	with sqrtl = (floor (sqrt l))
	for m from 2 below sqrtl by 2
	do (loop for n from 1 below (- sqrtl m) by 2
		 when (= 1 (gcd m n))
		   do (loop with p = (+ (abs (- (* m m) (* n n)))
					(* 2 m n)
					(+ (* m m) (* n n)))
			    for s from p upto l by p
			    do (incf (aref counts s))))
	finally (return (loop for i across counts counting (= i 1)))))
