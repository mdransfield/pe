;;;; Problem 206: Concealed Square
;;;;
;;;; Find the unique positive integer whose square has the form
;;;; 1_2_3_4_5_6_7_8_9_0, where each “_” is a single digit.

(in-package #:pe)

;; Squares ending 0 must end 00 and the root must end 0 (e.g. 20→400),
;; so we can discard those two digits, look for a smaller root and
;; multiply it by 10.
;;
;; We're now looking for a number 1_2_3_4_5_6_7_8_9.  Therefore the
;; largest number we're looking for is 19293949596979899.  Similarly,
;; the smallest number we're looking for is 10203040506070809.  This
;; bounds to the root.
;;
;; The only way to get a square ending in 9 is with a root ending in 3
;; or 7 but it's easier to look for an odd number, rather than trying
;; to fiddle with the interval in the loop.

(defparameter upper 439248787
  "Upper bound on solution.")
(defparameter lower 101010101
  "Lower bound on solution.")

(defun matchesp (n)
  (let ((d (digits-of n)))
    (and (= 17 (length d))
	 (= 1 (nth 0 d))
	 (= 2 (nth 2 d))
	 (= 3 (nth 4 d))
	 (= 4 (nth 6 d))
	 (= 5 (nth 8 d))
	 (= 6 (nth 10 d))
	 (= 7 (nth 12 d))
	 (= 8 (nth 14 d))
	 (= 9 (nth 16 d)))))

(defun euler-206 ()
  (* 10 (loop for x from lower upto upper by 2
	      until (matchesp (expt x 2))
	      finally (return x))))
