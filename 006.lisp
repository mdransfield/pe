
;; Find the difference between the sum of the squares of the first one
;; hundred natural numbers and the square of the sum.

(defun nums (n)
  (loop for i from 1 to n
        collect i))

(defun square (x)
  (* x x))

(defun squares (nums)
  (mapcar #'square nums))

(defun sum (nums)
  (reduce #'+ nums))

(defun euler-006 ()
  (let ((nums (nums 100)))
    (abs (- (sum (squares nums))
	    (square (sum nums))))))

;(time (euler-006))
