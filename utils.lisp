;;; Miscellaneous utility functions

(in-package :PE)

(defparameter factorial-table
  (make-hash-table)
  "A hash-table for the storage of previously calculated factorials.")

(defun factorial (n)
  "Calculate the factorial of N.
Skip the actual calculation if it's present in the accompanying hash-table."
  (labels ((ifact (n x)
	     (if (zerop n)
		 x
		 (ifact (1- n) (* n x)))))
    (multiple-value-bind (val win) (gethash n factorial-table)
      (if win
	  val
	  (setf (gethash n factorial-table) (ifact n 1))))))

(defun digits-of (n)
  "Return an ordered list of the digits of N."
  (if (zerop n)
      (list 0)
      (loop with temp = nil
	    while (> n 0)
	    do (multiple-value-bind (q r) (floor n 10)
		 (push r temp)
		 (setf n q))
	    finally (return temp))))

(defun from-digits (d)
  "Return a number composed of the digits in D."
  (loop with n = 0
	for i in d
	do (setf n (+ (* n 10) i))
	finally (return n)))

(defun permutations-of (s)
  "Generate all the permutations of S."
  (loop for i from 0 below (factorial (length s))
	collect (permutation i (make-array (length s) :initial-contents s))))

(defun permutation (k s)
  "Generate the Kth lexicographic permutation of S."
  (labels ((get-tempi (k f l i)
	     (mod (floor k f) (- l i))))
    (loop with l = (length s)
	  with fact = (factorial (1- l))
	  for i from 0 below (1- l)
	  for tempi = (get-tempi k fact l i) then (get-tempi k fact l i)
	  for temp = (aref s (+ i tempi)) then (aref s (+ i tempi))
	  do (loop for j = (+ i tempi) then (1- j)
		   while (> j i)
		   do (setf (aref s j) (aref s (1- j))))
	     (setf (aref s i) temp)
	     (setf fact (floor fact (- l i 1))))
    s))

(defun pandigitalp (digits)
  "Predicate true when DIGITS is 1-9 pandigital.
That is, DIGITS contains all the digits 1 to 9, but not necessarily in order."
  (and (eql 9 (length digits))
       (equal (sort (copy-seq digits) #'<) '(1 2 3 4 5 6 7 8 9))))

(defun modular-pow (base exponent modulus)
  "Calculate (mod (expt BASE EXPONENT) MODULUS) efficiently.

Algorithm for modular exponentation, from Schneier B., 1996, Applied
Cryptography: Protocols, Algorithms and Source Code in C.

function modular_pow(base, exponent, modulus) is
    if modulus = 1 then
        return 0
    Assert :: (modulus - 1) * (modulus - 1) does not overflow base
    result := 1
    base := base mod modulus
    while exponent > 0 do
        if (exponent mod 2 == 1) then
            result := (result * base) mod modulus
        exponent := exponent >> 1
        base := (base * base) mod modulus
    return result
"
  (if (= 1 modulus)
      0
      (loop with result = 1
	    with base = (mod base modulus)
	    while (> exponent 0)
	    if (= (mod exponent 2) 1)
	      do (setf result (mod (* result base) modulus))
	    do (setf exponent (ash exponent -1)
		     base (mod (* base base) modulus))
	       finally (return result))))

(defun reversed (n)
  "Return the reverse of N.
That is the number having digits of N in reverse order"
  (assert (integerp n))
  (parse-integer (reverse (princ-to-string n))))

(defun palindromep (n)
  "Predicate true when N is a palindrome.
That is, when its digits are the same read from either end."
  (eql n (reversed n)))
