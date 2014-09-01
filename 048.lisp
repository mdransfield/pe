
;;; Find the last ten digits of the series, 
;;; 1^(1) + 2^(2) + 3^(3) + ... + 1000^(1000).

(in-package #:pe)

(defun euler-048 ()
  (mod (loop for i from 1 to 1000
             sum (expt i i)) (expt 10 10)))