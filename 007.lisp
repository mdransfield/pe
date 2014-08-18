
;;; What is the 10001st prime number?

(in-package #:pe)

(defun euler-007 ()
  (nth 10000 (p:primes-upto 105000)))

;(time (euler-007))
