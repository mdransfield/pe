;;; The prime 41, can be written as the sum of six consecutive primes:
;;;     41 = 2 + 3 + 5 + 7 + 11 + 13
;;;
;;; This is the longest sum of consecutive primes that adds to a prime
;;; below one-hundred.
;;;
;;; The longest sum of consecutive primes below one-thousand that adds
;;; to a prime, contains 21 terms, and is equal to 953.
;;;
;;; Which prime, below one-million, can be written as the sum of the
;;; most consecutive primes?

(in-package #:pe)

(defun next-prime (n)
  (loop for i = (1+ n) then (1+ i)
        if (> i limit) return nil
        until (= 1 (bit primes i))
        finally (return i)))

(defun consecutive-prime-sums (start lim)
  (let ((sums (make-array 10
			   :element-type 'fixnum
			   :initial-element 0
			   :adjustable t
			   :fill-pointer 0)))
    (setf (aref sums 1) start)
    (loop for p = start then (next-prime p)
          for sum = start then (+ sum p)
	  while (< sum lim)
	  do (vector-push-extend sum sums))
    sums))

(defun longest-prime-sum-below (limit)
  (loop with longest = 0
        with biggest = 0
        for p = 2 then (next-prime p)
        for sums = (consecutive-prime-sums p limit)
        for len = (length sums)
        while (> len longest)
        do (loop for i from (1- len) downto 0
		 for test = (aref sums i)
		 for is-prime = (primep test)
		 when (and is-prime (> (1+ i) longest))
		 do (setf biggest test
			  longest (1+ i))
		 until is-prime)
        finally (return (list biggest longest))))

(defun euler-050 ()
  (longest-prime-sum-below 1000000))

;(time (euler-050 1000000))
