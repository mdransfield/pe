;;;; Problem 66: Diophantine equation
;;
;; Consider quadratic Diophantine equations of the form:
;;
;; x^2–Dy^2=1
;;
;; For example, when D=13, the minimal solution in x is
;; 6492–13×1802=1.
;;
;; It can be assumed that there are no solutions in positive integers when D is square.
;;
;; By finding minimal solutions in x for D = {2, 3, 5, 6, 7}, we
;; obtain the following:
;;
;; 3^2–2×2^2=1
;; 2^2–3×1^2=1
;; 9^2–5×4^2=1
;; 5^2–6×2^2=1
;; 8^2–7×3^2=1
;;
;; Hence, by considering minimal solutions in x for D≤7, the largest x
;; is obtained when D=5.
;;
;; Find the value of D≤1000 in minimal solutions of x for which the
;; largest value of x is obtained.

(in-package :cl-user)
(in-package :pe)

(defun pell-solution-p (D x y)
  (= 1 (- (expt x 2) (* D (expt y 2)))))

(defun solve-pell (D)
  "Solve Pell's equation for case D."
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
    (if (pell-solution-p D (aref p 1) (aref q 1))
	(/ (aref p 1) (aref q 1))
	(loop for n from 2
	      do (vector-push-extend (- (* (aref a (1- n)) (aref qq (1- n))) (aref pp (1- n))) pp)
		 (vector-push-extend (/ (- D (expt (aref pp n) 2)) (aref qq (1- n))) qq)
		 (vector-push-extend (floor (/ (+ (aref a 0) (aref pp n)) (aref qq n))) a)
		 (vector-push-extend (+ (* (aref a n) (aref p (1- n))) (aref p (- n 2))) p)
		 (vector-push-extend (+ (* (aref a n) (aref q (1- n))) (aref q (- n 2))) q)
	  until (pell-solution-p D (aref p n) (aref q n))
	      finally (return (/ (aref p n) (aref q n)))))))

(defun solve-pell-for-x (D)
  (numerator (solve-pell D)))

(defun euler-066 ()
  (loop with x-max = 0
	with D-x-max = 0
	for D from 2 upto 1000
	for x = (if (/= (expt (isqrt D) 2) D)
		    (solve-pell-for-x D)
		    0)
	when (> x x-max)
	  do (setf x-max x
		   D-x-max D)
	finally (return (cons D-x-max x-max))))
