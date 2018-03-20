(in-package #:pe)

(defparameter *sudoku-filepath* #P"/home/mdransfield/quicklisp/local-projects/pe/files/p096_sudoku.txt")

(defun euler-096 ()
  (reduce #'+
	  (mapcar (lambda (e)
		    (+ (* 100 (aref e 0))
		       (*  10 (aref e 1))
		       (aref e 2)))
		  (with-open-file (f *sudoku-filepath*)
		    (loop for i from 0 below 50
		       collect (subseq (sudoku:solver (list (read-sudoku f))) 0 3))))))

(defun read-sudoku (s)
  (map 'vector
       (lambda (c)
	 (- (char-code c) (char-code #\0)))
       (apply #'concatenate 'string
	      (loop with header = (read-line s)
		 for j from 0 below 9
		 collect (read-line s)))))
