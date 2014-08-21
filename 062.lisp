
;;; Cubic permutations
;;
;; The cube, 41063625 (345^3), can be permuted to produce two other
;; cubes: 56623104 (384^3) and 66430125 (405^3). In fact, 41063625 is
;; the smallest cube which has exactly three permutations of its
;; digits which are also cube.
;;
;; Find the smallest cube for which exactly five permutations of its
;; digits are cube.

(in-package #:pe)

(defun euler-062 ()
  (loop with map = (make-hash-table :test #'equal)
     for i from 2 upto 10000
     do (push i (gethash (sort (prin1-to-string (expt i 3)) #'char<) map))
     finally (return (loop for v being the hash-values of map
			when (= 5 (length v))
			return (expt (car (sort v #'<)) 3)))))
