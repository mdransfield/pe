
;;; The following iterative sequence is defined for the set of
;;; positive integers:
;;;
;;; n -->  n/2   (n is even)
;;; n --> 3n + 1 (n is odd)
;;;
;;; Which starting number, under one million, produces the longest
;;; chain?

(in-package #:pe)

(defun make-table ()
  (let ((table (make-hash-table)))
    (setf (gethash 1 table) 1
	  (gethash 2 table) 2
	  (gethash 4 table) 3
	  (gethash 8 table) 4)
    table))

(defun next-in-sequence (n)
  (if (zerop (mod n 2))
      (/ n 2)
      (1+ (* 3 n))))

(defun set-length (n table)
  (loop for count = 1 then (1+ count)
        for i = (next-in-sequence n) then (next-in-sequence i)
        for hash-val = (gethash i table) then (gethash i table)
        if hash-val do (setf (gethash n table) (+ count hash-val))
        until hash-val))

(defun get-lengths (table)
  (loop for i from 3 below 1000000
        do (set-length i table)
        when (zerop (mod i 1000)) do (format t "."))
  table)

(defun longest (table)
  (let ((max-key 0)
	(max-val 0))
    (maphash (lambda (k v)
	       (when (> v max-val)
		 (setf max-key k
		       max-val v))) table)
    (values max-key max-val)))

(defun euler-014 ()
  (longest (get-lengths (make-table))))

;(time (euler-014))
