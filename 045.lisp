
;;; Triangular, pentagonal, and hexagonal
;;
;; After 40755, what is the next triangle number that is also
;; pentagonal and hexagonal?

(in-package #:pe)

(defun triagonal (limit hash)
  (loop for n from 1 to limit
        for tri = (* 1/2 n (1+ n))
        do (incf (gethash tri hash 0))))

(defun pentagonal (limit hash)
  (loop for n from 1 to limit
        for pen = (* 1/2 n (1- (* 3 n)))
        do (incf (gethash pen hash 0))))

(defun hexagonal (limit hash)
  (loop for n from 1 to limit
        for hex = (* n (1- (* 2 n)))
        do (incf (gethash hex hash 0))))

(defun euler-045 ()
  (let ((limit 100000)
	(hash (make-hash-table)))
    (triagonal limit hash)
    (pentagonal limit hash)
    (hexagonal limit hash)
    (loop for key being each hash-key of hash
	  for val = (gethash key hash)
	  when (= 3 val) collect key)))

;(time (euler-045))
