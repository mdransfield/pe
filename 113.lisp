;;;; Problem 113: Non-bouncy numbers
;;
;; Working from left-to-right if no digit is exceeded by the digit
;; to its left it is called an increasing number; for example,
;; 134468.
;;
;; Similarly if no digit is exceeded by the digit to its right it is
;; called a decreasing number; for example, 66420.
;;
;; We shall call a positive integer that is neither increasing nor
;; decreasing a "bouncy" number; for example, 155349.
;;
;; As _n_ increases, the proportion of bouncy numbers below n
;; increases such that there are only 12951 numbers below
;; one-million that are not bouncy and only 277032 non-bouncy
;; numbers below 10^10.
;;
;; How many numbers below a googol (10^100) are not bouncy?

(in-package #:pe)

(defun choose (n k)
  "Number of ways of choosing K elements from a N-element set."
  (/ (factorial n) (factorial k) (factorial (- n k))))

(defun euler-113 ()
  (let* ((n 100)
	 (increasing (1- (choose 110 10)))
	 (decreasing (1- (choose 109 9)))
	 (single (* n 10)))
    (+ increasing decreasing (- single))))
