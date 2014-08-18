
;;; How many different ways can £2 be made using any number of coins?

(in-package #:pe)

(defun make-cache (coins max)
  (loop with cache = (make-hash-table)
        for c in coins
        do (setf (gethash c cache) (make-array (1+ max) :initial-element 0)
		 (aref (gethash c cache) 0) 1)
        finally (return cache)))

(defun ways (n coins cache)
  (let ((coin (car coins)))
    (cond
      ((null coin) 0)
      ((< n 0) 0)
      (t (let ((w (aref (gethash coin cache) n)))
	   (if (> w 0)
	       w
	       (setf (aref (gethash coin cache) n)
		     (+ (ways n (cdr coins) cache)
			(ways (- n coin) coins cache)))))))))

(defun euler-031 ()
  (let ((coins '(200 100 50 20 10 5 2 1)))
    (ways 200 coins (make-cache coins 200))))

;(time (euler-031))
