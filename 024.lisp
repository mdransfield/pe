
;;; What is the millionth lexicographic permutation of the digits 0,
;;; 1, 2, 3, 4, 5, 6, 7, 8 and 9?

(in-package #:pe)

(defun euler-024 ()
  (permutation 999999 (make-array 10 :initial-contents '(0 1 2 3 4 5 6 7 8 9))))

;(time (euler-024))
