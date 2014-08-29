;;; Pentagon numbers
;;
;; Find the pair of pentagonal numbers, Pj and Pk, for which their
;; sum and difference is pentagonal and D = |Pk-Pj| is minimised;
;; what is the value of D?

(in-package #:pe)

(defun pentagonalp (n)
  (zerop (mod (1+ (sqrt (1+ (* 24 n)))) 6)))

(defun next-pentagonal (n)
  (/ (* n (1- (* 3 n))) 2))

(defun euler-044 ()
  (loop with pents = (make-array 3000 :element-type 'integer)
        with result = most-positive-fixnum
        for i from 0 below 3000
        do (setf (aref pents i) (next-pentagonal (1+ i)))
           (loop for j from 0 below i
                 for sum = (+ (aref pents i) (aref pents j))
                 for dif = (- (aref pents i) (aref pents j))
                 when (and (pentagonalp sum)
                           (pentagonalp dif)
                           (< dif result))
                 do (setf result dif))
       finally (return result)))
