;;;; Problem 303: Multiples with small digits
;;
;; For a positive integer n, define f(n) as the least positive
;; multiple of n that, written in base 10, uses only digits ≤ 2.
;;
;; Thus f(2)=2, f(3)=12, f(7)=21, f(42)=210, f(89)=1121222.
;;
;; Also, \sum \limits_{n = 1}^{100} {\dfrac{f(n)}{n}} = 11363107.
;;
;; Find \sum \limits_{n=1}^{10000} {\dfrac{f(n)}{n}}.

(in-package #:PE)

(defun euler-303 (&optional (limit 10000))
  (let ((f (make-array limit :element-type 'bit))
	(s 0))
    (when (< 9999 limit)
      (setf (bit f 9998) 1)
      (incf s (/ 11112222222222222222 9999)))
    (loop for x10 from 1
	  for x = (read-from-string (format nil "~3r" x10))
	  do (loop for n = (position 0 f) then (position 0 f :start (1+ n))
		   while (and (numberp n) (<= n x))
		   when (zerop (mod x (1+ n)))
		     do (setf (bit f n) 1)
			(incf s (/ x (1+ n))))
	  until (null (position 0 f)))
    s))
