;;;; Problem 94: Almost equilateral triangles
;;
;; It is easily proved that no equilateral triangle exists with
;; integral length sides and integral area. However, the almost
;; equilateral triangle 5-5-6 has an area of 12 square units.
;;
;; We shall define an almost equilateral triangle to be a triangle for
;; which two sides are equal and the third differs by no more than one
;; unit.
;;
;; Find the sum of the perimeters of all almost equilateral triangles
;; with integral side lengths and area and whose perimeters do not
;; exceed one billion (1,000,000,000).

(in-package #:pe)

;;; Following https://oeis.org/A102341 and the comment about Heron's
;;; formula.

(defun pell-convergents (D x)
  "Generate successive convergents for Pell's equation for case D."
  (let ((pp (make-array 0 :adjustable t :fill-pointer t))
	(qq (make-array 0 :adjustable t :fill-pointer t))
	(a (make-array 0 :adjustable t :fill-pointer t))
	(p (make-array 0 :adjustable t :fill-pointer t))
	(q (make-array 0 :adjustable t :fill-pointer t)))
    (vector-push-extend 0 pp)
    (vector-push-extend 1 qq)
    (vector-push-extend (isqrt D) a)
    (vector-push-extend (aref a 0) p)
    (vector-push-extend 1 q)
    (vector-push-extend (aref a 0) pp)
    (vector-push-extend (- D (expt (aref a 0) 2)) qq)
    (vector-push-extend (floor (/ (+ (aref a 0) (aref pp 1)) (aref qq 1))) a)
    (vector-push-extend (1+ (* (aref a 0) (aref a 1))) p)
    (vector-push-extend (aref a 1) q)
    (append (list (/ (aref p 0) (aref q 0)) (/ (aref p 1) (aref q 1)))
	    (loop for n from 2 upto x
		  do (vector-push-extend (- (* (aref a (1- n)) (aref qq (1- n))) (aref pp (1- n))) pp)
		     (vector-push-extend (/ (- D (expt (aref pp n) 2)) (aref qq (1- n))) qq)
		     (vector-push-extend (floor (/ (+ (aref a 0) (aref pp n)) (aref qq n))) a)
		     (vector-push-extend (+ (* (aref a n) (aref p (1- n))) (aref p (- n 2))) p)
		     (vector-push-extend (+ (* (aref a n) (aref q (1- n))) (aref q (- n 2))) q)
		  collect (/ (aref p n) (aref q n))))))

(defun area (a b)
  "The area of an isosceles triangle with sides A, A, & B."
  (cr:*r (cr:/r b 4) (cr:sqrt-r (cr:-r (cr:*r 4 (cr:expt-r a 2)) (cr:expt-r b 2)))))

(defun euler-081 ()
  (loop for c in (mapcar (lambda (x) (numerator x)) (pell-convergents 3 40))
	for k+ = (* 2/3 (+ c 2))
	for k- = (* 2/3 (- c 2))
	for k  = (if (integerp k+) k+ k-)
	for a+ = (area (1+ k) k)
	for a- = (area (1- k) k)
	for p  = (if (and (integerp a+) (> a+ 0)) (+ (* 2 (1+ k)) k)
		     (if (and (integerp a-) (> a- 0)) (+ (* 2 (1- k)) k)
			 0))
	while (<= p 1000000000)
	sum p))
