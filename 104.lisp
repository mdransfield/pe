;; The Fibonacci sequence is defined by the recurrence relation:
;;
;; F_n = F_n−1 + F_n−2, where F_1 = 1 and F_2 = 1.
;;
;; It turns out that F_541, which contains 113 digits, is the first
;; Fibonacci number for which the last nine digits are 1-9 pandigital
;; (contain all the digits 1 to 9, but not necessarily in order). And
;; F_2749, which contains 575 digits, is the first Fibonacci number
;; for which the first nine digits are 1-9 pandigital.
;;
;; Given that F_k is the first Fibonacci number for which the first
;; nine digits AND the last nine digits are 1-9 pandigital, find k.


(defconstant +logsqrt5+ (cr:log-r f:+sqrt5+ 10))

(defconstant +logphi+ (cr:log-r f:+phi+ 10))

(defun first-10-digits (n)
  (values (cr:floor-r (cr:expt-r 10 (cr:+r 8 (cadr (multiple-value-list (cr:floor-r (cr:-r (cr:*r n +logphi+) +logsqrt5+)))))))))
  
(defun euler-104 ()
    (loop with n = 2
	  and f2 = 1
	  and f1 = 1
	  and f = 1
	  do (incf n)
	     (setf f (mod (+ f2 f1) 1000000000))
	     (setf f2 f1)
	     (setf f1 f)
	  until (and (pandigitalp f) (pandigitalp (first-10-digits n)))
	  finally
	     (return n)))
