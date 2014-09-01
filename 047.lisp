
;;; Find the first four consecutive integers to have four distinct
;;; primes factors.  What is the first of these numbers?

(in-package #:pe)

(defparameter limit 1000000)

(defparameter primes (p::make-sieve limit))

(defun next-prime (n)
  (loop for i = (1+ n) then (1+ i)
        if (> i limit) do (error "Out of primes")
        until (= 1 (bit primes i))
        finally (return i)))

(defun primep (n)
  (= 1 (bit primes n)))

(defun prime-factors (n)
  (loop with factors = nil
        for p = 2 then (next-prime p)
        if (zerop (mod n p))
        do (push p factors)
           (loop do (setf n (floor n p))
		 while (zerop (mod n p)))
           if (primep n)
           do (push n factors)
              (return factors)
        until (= 1 n)
        finally (return factors)))

(defun euler-047 ()
  (loop with i = 134040
        if (and (= 4 (length (prime-factors i)))
                (= 4 (length (prime-factors (incf i))))
                (= 4 (length (prime-factors (incf i))))
		(= 4 (length (prime-factors (incf i)))))
           return (- i 3)
        else do (incf i)))

;(time (euler-047))
