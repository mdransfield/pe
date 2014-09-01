
;;; The arithmetic sequence, 1487, 4817, 8147, in which each of the
;;; terms increases by 3330, is unusual in two ways: (i) each of the
;;; three terms are prime, and, (ii) each of the 4-digit numbers are
;;; permutations of one another.
;;;
;;; There are no arithmetic sequences made up of three 1-, 2-, or
;;; 3-digit primes, exhibiting this property, but there is one other
;;; 4-digit increasing sequence.
;;;
;;; What 12-digit number do you form by concatenating the three terms
;;; in this sequence?

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

(defun digits (n)
  (loop with temp = nil
        while (> n 0)
        do (multiple-value-bind (q r) (floor n 10)
	     (push r temp)
	     (setf n q))
        finally (return temp)))

(defun perm (n)
  (labels ((new (a b c d)
             (+ (* 1000 a) (* 100 b) (* 10 c) d)))
    (let* ((x (digits n))
	   (a (first x))
	   (b (second x))
	   (c (third x))
	   (d (fourth x))
           perms)
      (pushnew (new a b c d) perms)
      (pushnew (new a b d c) perms)
      (pushnew (new a c b d) perms)
      (pushnew (new a c d b) perms)
      (pushnew (new a d b c) perms)
      (pushnew (new a d c b) perms)
      (pushnew (new b a c d) perms)
      (pushnew (new b a d c) perms)
      (pushnew (new b c a d) perms)
      (pushnew (new b c d a) perms)
      (pushnew (new b d a c) perms)
      (pushnew (new b d c a) perms)
      (pushnew (new c a b d) perms)
      (pushnew (new c a d b) perms)
      (pushnew (new c b a d) perms)
      (pushnew (new c b d a) perms)
      (pushnew (new c d a b) perms)
      (pushnew (new c d b a) perms)
      (pushnew (new d a c b) perms)
      (pushnew (new d a b c) perms)
      (pushnew (new d c a b) perms)
      (pushnew (new d c b a) perms)
      (pushnew (new d b a c) perms)
      (pushnew (new d b c a) perms)
      (nreverse perms))))

(defun arithmetic-sequence-p (seq)
  (and (>= (length seq) 3)
       (= (- (second seq) (first seq))
          (- (third seq) (second seq)))))

(defun euler-049 ()
  (loop for i = (next-prime 1000) then (next-prime (car l))
        for l = (remove-if-not (lambda (x) (and (> x 1000)
                                                 (primep x))) (perm i))
        until (arithmetic-sequence-p l)
        finally (return l)))

;(remove-if-not (lambda (x) (and (> x 1000) (primep x))) (perm (next-prime 1009)))