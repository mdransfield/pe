;;; Problem 179: Consecutive positive divisors
;;
;; Find the number of integers 1<n<10⁷, for which n and n+1 have the
;; same number of positive divisors. For example, 14 has the positive
;; divisors 1, 2, 7, 14 while 15 has 1, 3, 5, 15.

(in-package #:pe)

(defun trial-division (n)
  "Find the prime factors of N by trial division."
  (let ((a ())
	(f 3))
    (loop while (zerop (mod n 2))
	  do (push 2 a)
	     (setf n (/ n 2)))
    (loop while (<= (expt f 2) n)
	  if (zerop (mod n f))
	    do (push f a)
	       (setf n (/ n f))
	  else
	    do (incf f 2))
    (unless (= n 1)
      (push n a))
    a))

(defun prime-factors-of (n)
  (loop with raw = (trial-division n)
	with cache = (make-hash-table)
	for f in raw
	do (setf (gethash f cache) (1+ (gethash f cache 0)))
	finally (return (loop for k being the hash-keys in cache
				using (hash-value v)
			      collect (cons k v)))))

(defun sigma0 (n)
  (loop with sigma0 = 1
	for f in (prime-factors-of n)
	do (setf sigma0 (* sigma0 (1+ (cdr f))))
	finally (return sigma0)))

(defun euler-179 (&optional (limit (expt 10 7)))
  (loop for n from 2 below limit
	for s0n = (sigma0 n) then s0n1
	for s0n1 = (sigma0 (1+ n))
	when (= s0n s0n1)
	  count (= s0n s0n1)))
