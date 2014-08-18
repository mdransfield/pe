
;; Add all the natural numbers below one thousand that are multiples
;; of 3 or 5.

(defun multiple-of-p (n x)
  (eql 0 (mod x n)))

(defun requiredp (n)
  (or (multiple-of-p 3 n)
      (multiple-of-p 5 n)))

(defun natural-numbers-less-than (n)
  (loop for i from 1 below n
        collect i))

(defun euler-001 ()
  (reduce #'+ (remove-if-not #'requiredp (natural-numbers-less-than 1000))))

;(euler-001)