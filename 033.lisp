
;;; The fraction 49/98 is a curious fraction, as an inexperienced
;;; mathematician in attempting to simplify it may incorrectly believe
;;; that 49/98 = 4/8, which is correct, is obtained by cancelling the
;;; 9s.
;;;
;;; We shall consider fractions like, 30/50 = 3/5, to be trivial
;;; examples.
;;;
;;; There are exactly four non-trivial examples of this type of
;;; fraction, less than one in value, and containing two digits in the
;;; numerator and denominator.
;;;
;;; If the product of these four fractions is given in its lowest
;;; common terms, find the value of the denominator.

(in-package #:pe)

(defun curiousp (a b c)
  (eql (/ a c) (/ (+ (* 10 a) b) (+ (* 10 b) c))))

(defun curious-nums ()
  (loop with curios = nil
        for a from 1 to 9
        do (loop for b from 1 to 9
                 do (loop for c from 1 to 9
                          when (and (/= a b c) (curiousp a b c))
                          do (push (/ a c) curios)))
        finally (return curios)))

(defun euler-033 ()
  (floor (/ (reduce #'* (curious-nums)))))

;(time (euler-033))
