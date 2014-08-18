
;;; How many Sundays fell on the first of the month during the
;;; twentieth century (1 Jan 1901 to 31 Dec 2000)?

(in-package #:pe)

(defun leap-year-p (year)
  (and (zerop (mod year 4))
       (or (not (zerop (mod year 100)))
	   (zerop (mod year 400)))))

(defun day-number (date)
  (let* ((year (car date))
	 (month (cadr date))
	 (day  (caddr date))
	 (day-of-year (+ day (* 31 (1- month)))))
    (if (> month 2)
	(progn
	  (setf day-of-year (- day-of-year (floor (+ 23 (* 4 month)) 10)))
	  (if (leap-year-p year)
	      (incf day-of-year))))
    day-of-year))

(defun absolute-from-gregorian (date)
  (let* ((year (car date))
	 (offset-year (1- year)))
    (+ (day-number date)
       (* 365 offset-year)
       (floor offset-year 4)
       (- (floor offset-year 100))
       (floor offset-year 400))))

(defun day-of-week (date)
  (mod (absolute-from-gregorian date) 7))

(defun euler-019 ()
  (loop with count = 0
        for y from 1901 to 2000
        do (loop for m from 1 to 12
		 when (zerop (day-of-week (list y m 1)))
	         do (incf count)
		    (format t "~&~d ~d ~d~%" y m 1))
        finally (return count)))

;(time (euler-019))
