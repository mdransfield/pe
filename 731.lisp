;;;; Probem 731: A Stoneham number
;;
;; A=\sum_{i=1}^{\infty} \frac{1}{3^i 10^{3^i}}
;;
;; Define A(n) to be the 10 decimal digits from the th digit
;; onward. For example, A(100)=4938271604 and A(10^8)=2584642393.
;;
;; Find A(10^16)

(in-package :pe)

(defun euler-731 ()
  (let ((val (loop with lim = (expt 10 16)
		   with m = (floor (log lim 3))
		   for k from 1 upto (1+ m)
		   for d = (expt 3 k)
		   for e = (- lim d 1)
		   sum (/ (modular-pow 10 e d) d))))
    (multiple-value-bind (q r)
	(floor (coerce val 'double-float))
      (declare (ignore q))
      (values (floor (* r (expt 10 10)))))))
