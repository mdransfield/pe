;;; It is possible to show that the square root of two can be
;;; expressed as an infinite continued fraction.
;;;
;;; sqrt(2) = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...
;;;
;;; By expanding this for the first four iterations, we get:
;;;
;;; 1 + 1/2 = 3/2 = 1.5
;;; 1 + 1/(2 + 1/2) = 7/5 = 1.4
;;; 1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
;;; 1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...
;;; 
;;; The next three expansions are 99/70, 239/169, and 577/408, but the
;;; eighth expansion, 1393/985, is the first example where the number
;;; of digits in the numerator exceeds the number of digits in the
;;; denominator.
;;; 
;;; In the first one-thousand expansions, how many fractions contain a
;;; numerator with more digits than denominator?

(in-package #:pe)

(defun generate-fraction (n)
  (if (= 0 n)
      '(/ 1 2)
      `(/ 1 (+ 2 ,(generate-fraction (1- `,n))))))

(defun generate-sum (n)
  (eval `(+ 1 ,(generate-fraction n))))

(defun num-digits (n)
  (1+ (floor (log n 10))))

(defun wantedp (n)
  (> (num-digits (numerator n)) (num-digits (denominator n))))

(defun euler-057 ()
  (loop for n from 0 below 1000
     count (wantedp (generate-sum n))))

;(time (euler-057))
