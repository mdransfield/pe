
;; Find the largest palindrome made from the product of two 3-digit numbers.

(defun reversed (n)
  (loop with r = 0
        while (> n 0) do (setf r (+ (* 10 r) (mod n 10))
			       n (floor (/ n 10)))
        finally (return r)))

(defun palindromep (n)
  (eql n (reversed n)))

(defun euler-004 ()
  (let ((max 0)
	(x 0)
	(y 0))
    (loop for i from 999 downto 100
          do (loop for j from 999 downto i
		   for p = (* i j) then (* i j)
		   if (and (> p max)
			   (palindromep p))
		   do (setf max p
			    x i
			    y j)))
    (values x y max)))

;(time (euler-004))