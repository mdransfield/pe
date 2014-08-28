
;;; Integer right triangles
;;
;; If p is the perimeter of a right angle triangle with integral
;; length sides, {a,b,c}, there are exactly three solutions for
;; p = 120.
;;
;; {20,48,52}, {24,45,51}, {30,40,50}
;;
;; For which value of p <= 1000, is the number of solutions maximised?

(in-package #:pe)

(defparameter triples (with-open-file (s "files/pythagorean-triples.lisp")
			(read s)))

(defun populate-perimeters (triples)
  (loop with table = (make-hash-table)
        for r in triples
        for p = (apply #'+ r)
        while (<= p 1000)
        do (multiple-value-bind (cnt win) (gethash p table)
	     (declare (ignorable cnt))
	     (if win
		 (incf (gethash p table))
		 (setf (gethash p table) 1)))
        finally (return table)))

(defun table-to-list (table)
  (loop for k being each hash-key in table
              using (hash-value v)
        collect (list k v)))

(defun find-max-perimeter (perimeters)
  (sort (table-to-list perimeters) #'> :key #'cadr))

(defun euler-039 ()
  (car (find-max-perimeter (populate-perimeters triples))))
