;;;; Proble 700: Eulercoin
;;;;
;;;; Leonhard Euler was born on 15 April 1707.
;;;;
;;;; Consider the sequence 1504170715041707n mod 4503599627370517.
;;;;
;;;; An element of this sequence is defined to be an Eulercoin if it
;;;; is strictly smaller than all previously found Eulercoins.
;;;;
;;;; For example, the first term is 1504170715041707 which is the
;;;; first Eulercoin. The second term is 3008341430083414 which is
;;;; greater than 1504170715041707 so is not an Eulercoin. However,
;;;; the third term is 8912517754604 which is small enough to be a new
;;;; Eulercoin.
;;;;
;;;; The sum of the first 2 Eulercoins is therefore 1513083232796311.
;;;;
;;;; Find the sum of all Eulercoins.

(in-package #:pe)

(defparameter initial 1504170715041707)
(defparameter modulus 4503599627370517)

(defun euler-700-bf ()
  (loop with initial = 1504170715041707
	with modulus = 4503599627370517
	with coins = nil
	with sum = 0
	for n from  1 upto modulus
	for cand = initial then (mod (+ cand initial) modulus)
	  when (every (lambda (x) (< cand x)) coins)
	    do (incf sum cand)
	  finally (return sum)))

(defun euler-700 ()
  (loop with hi = initial
	with lo = initial
	with sum = initial
	for next = (mod (+ lo hi) modulus)
	while (> lo 0)
	if (< next lo)
	  do (setf lo next)
	     (incf sum lo)
	else
	  do (setf hi next)
	finally (return sum)))
