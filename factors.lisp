;;;; Functions related to divisors and factorization

(in-package #:PE)

(defun trial-division (n)
  "The prime factors of N by trial division."
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
  "The prime factors of N.
The function returns a list of CONSes.  Each cell has the factor in
CAR and its power in CDR."
  (loop with raw = (trial-division n)
	with cache = (make-hash-table)
	for f in raw
	do (setf (gethash f cache) (1+ (gethash f cache 0)))
	finally (return (loop for k being the hash-keys in cache
				using (hash-value v)
			      collect (cons k v)))))

(defun sigma0 (n)
  "The divisor count σ₀(N)."
  (loop with sigma0 = 1
	for f in (prime-factors-of n)
	do (setf sigma0 (* sigma0 (1+ (cdr f))))
	finally (return sigma0)))
