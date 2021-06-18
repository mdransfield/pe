;;;; Problem 89: Roman numerals
;;
;; For a number written in Roman numerals to be considered valid there
;; are basic rules which must be followed.  Even though the rules
;; allow some numbers to be expressed in more than one way there is
;; always a "best" way of writing a particular number.
;;
;; For example, it would appear that there are at least six ways of
;; writing the number sixteen:
;;
;; IIIIIIIIIIIIIIII
;; VIIIIIIIIIII
;; VVIIIIII
;; XIIIIII
;; VVVI
;; XVI
;;
;; However, according to the rules only XIIIIII and XVI are valid, and
;; the last example is considered to be the most efficient, as it uses
;; the least number of numerals.
;;
;; The 11K text file, roman.txt (right click and 'Save Link/Target
;; As...'), contains one thousand numbers written in valid, but not
;; necessarily minimal, Roman numerals; see <a
;; href="https://projecteuler.net/about=roman_numerals">About... Roman Numerals</a>
;; for the definitive rules for this problem.
;;
;; Find the number of characters saved by writing each of these in
;; their minimal form.
;;
;; Note: You can assume that all the Roman numerals in the file
;; contain no more than four consecutive identical units.

(in-package #:pe)

(defparameter *numberlist-filepath* (merge-pathnames #P"files/p089_roman.txt"))

(defun read-numbers (filepath)
  (with-open-file (instream filepath)
    (loop for num = (read-line instream nil)
	  while num
	  collect num)))

(defun parse-roman (r)
  (flet ((mapcn (chars nums string)
	   (loop for char across string
	for i = (position char chars)
	when i
	  collect (nth i nums))))
    (loop with nums = (mapcn "IVXLCDM" '(1 5 10 50 100 500 1000) r)
	  for (a b) on nums
	  when a
	    sum (if (and b (< a b)) (- a) a))))

(defun roman-string (x)
  (loop with xr = ""
	for n in '(1000 900 500 400 100 90 50 40 10 9 5 4 1)
	for r in '("M" "CM" "D" "CD" "C" "XC" "L" "XL" "X" "IX" "V" "IV" "I")
	do (loop while (>= x n)
		 do (setf xr (concatenate 'string xr r))
		    (decf x n))
	  finally (return xr)))

(defun euler-089 ()
  (loop for n in (read-numbers *numberlist-filepath*)
	sum (- (length n) (length (roman-string (parse-roman n))))))
