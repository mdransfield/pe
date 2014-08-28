
;;; Prime pair sets
;;
;; The primes 3, 7, 109, and 673, are quite remarkable. By taking any
;; two primes and concatenating them in any order the result will
;; always be prime. For example, taking 7 and 109, both 7109 and 1097
;; are prime. The sum of these four primes, 792, represents the lowest
;; sum for a set of four primes with this property.
;;
;; Find the lowest sum for a set of five primes for which any two
;; primes concatenate to produce another prime.

(in-package #:pe)

(defun concatenablep (n1 n2)
  (and (p:primep (parse-integer (format nil "~d~d" n1 n2)))
       (p:primep (parse-integer (format nil "~d~d" n2 n1)))))

(defun concatentable-with (n p m)
  (if (gethash n m)
      (values (gethash n m))
      (setf (gethash n m)
	    (loop for x in p
		  when (and (> x n)
			    (concatenablep n x))
		    collect x))))

(defun euler-060 ()
  (loop with m = (make-hash-table)
	with p = (p:primes-upto 10000)
	for a in p
	for bs in (concatentable-with a p m)
	do (loop for b in bs
		 for cs = (intersection bs (concatenable-with b p m))
		 do (loop for c in cs
			  for ds = (intersection cs (concatenable-with c p m))
			  do (loop for e in es
				   do (return-from euler-060 (+ a b c d e)))))))
