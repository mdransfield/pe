;;; Circular primes
;;
;; The number, 197, is called a circular prime because all rotations
;; of the digits: 197, 971, and 719, are themselves prime.
;;
;; There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17,
;; 31, 37, 71, 73, 79, and 97.
;; 
;; How many circular primes are there below one million?

(in-package #:pe)

(defun make-sieve (n)
  (let ((sieve (make-array (1+ n) :element-type 'bit :initial-element 1)))
    (setf (bit sieve 1) 0)
    sieve))

(defun new-value (p sieve)
  (loop do (incf p)
        until (eql (bit sieve p) 1)
        finally (return p)))

(defun delete-multiples (n p sieve)
  (loop with j = (* p p)
        while (<= j n)
        do (setf (bit sieve j) 0)
        (incf j p)))

(defun sieve (sieve n)
  (loop with p = 2
        while (<= (* p p) n)
        do (delete-multiples n p sieve)
           (setf p (new-value p sieve))
        finally (return sieve)))

(defun next-rotation (n d)
  (if (/= d (ceiling (log n 10)))
      (* n 10)
    (multiple-value-bind (m r) (floor n (expt 10 (floor (log n 10))))
      (+ (* r 10) m))))

(defun rotations (n rotations)
  (setf (fill-pointer rotations) 0)
  (vector-push-extend n rotations)
  (loop with d = (ceiling (log n 10))
        for r = (next-rotation n d) then (next-rotation r d)
        while (/= r n) do (vector-push-extend r rotations)
        finally (return rotations)))

(defun circularp (rotations primes)
  (and (every #'oddp rotations)
       (notany (lambda (x) (= 5 (mod x 10))) rotations)
       (every (lambda (x) (= 1 (bit primes x))) rotations)))

(defparameter limit 1000000)

(defun next-prime (n primes)
  (loop for i from (1+ n) to limit
        when (= 1 (bit primes i)) return i))

(defun euler-035 ()
  (loop with primes = (sieve (make-sieve limit) limit)
        with count = 4
        with r = (make-array 6
			     :element-type 'fixnum
			     :adjustable t
			     :fill-pointer 0)
        for p = 11 then (next-prime p primes)
        while p
        do (when (circularp (rotations p r) primes)
	     (loop for i from 0 below (fill-pointer r)
		   do (setf (bit primes (aref r i)) 0)
		      (incf count)))
        finally (return count)))
                          

;(time (euler-035))
