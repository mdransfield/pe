
;;; Sub-string divisibility
;; 
;; Find the sum of all 0 to 9 pandigital numbers with this property:
;;
;; Let d0 be the 1st digit, d1 be the 2nd digit, and so on. Note the
;; following:
;;
;; test-1: d1 d2 d3 is divisible by 2
;; test-2: d2 d3 d4 is divisible by 3
;; test-3: d3 d4 d5 is divisible by 5
;; test-4: d4 d5 d6 is divisible by 7
;; test-5: d5 d6 d7 is divisible by 11
;; test-6: d6 d7 d8 is divisible by 13
;; test-7: d7 d8 d9 is divisible by 17

(in-package #:pe)

(defun test-17 ()
  (loop for i from 1 to 80
        for p = (* 17 i)
        for d1 = (floor p 100)
        for d2 = (floor (- p (* d1 100)) 10)
        for d3 = (mod p 10)
        while (< p 1000)
        if (/= d1 d2 d3) collect (list d1 d2 d3)))

(defun test-13 (t17s)
  (loop with t13s = nil
        for t17 in t17s
        for d4 = (third t17)
        for d3 = (second t17)
        for d2 = (first t17)
        do (loop for d1 from 0 to 9
                 if (and (/= d1 d2 d3 d4)
                         (zerop (mod (+ (* 100 d1) (* 10 d2) d3) 13)))
                 do (push (list d1 d2 d3 d4) t13s))
        finally (return t13s)))

(defun test-11 (t13s)
  (loop with t11s = nil
        for t13 in t13s
        for d5 = (fourth t13)
        for d4 = (third t13)
        for d3 = (second t13)
        for d2 = (first t13)
        do (loop for d1 from 0 to 9
                 if (and (/= d1 d2 d3 d4 d5)
                         (zerop (mod (+ (* 100 d1) (* 10 d2) d3) 11)))
                 do (push (list d1 d2 d3 d4 d5) t11s))
        finally (return t11s)))

(defun test-7 (t11s)
  (loop with t7s = nil
        for t11 in t11s
        for d6 = (fifth t11)
        for d5 = (fourth t11)
        for d4 = (third t11)
        for d3 = (second t11)
        for d2 = (first t11)
        do (loop for d1 from 0 to 9
                 if (and (/= d1 d2 d3 d4 d5 d6)
                         (zerop (mod (+ (* 100 d1) (* 10 d2) d3) 7)))
                 do (push (list d1 d2 d3 d4 d5 d6 ) t7s))
        finally (return t7s)))

(defun test-5 (t7s)
  (loop with t5s = nil
        for t7 in t7s
        for d7 = (sixth t7)
        for d6 = (fifth t7)
        for d5 = (fourth t7)
        for d4 = (third t7)
        for d3 = (second t7)
        for d2 = (first t7)
        do (loop for d1 from 0 to 9
                 if (and (/= d1 d2 d3 d4 d5 d6 d7)
                         (zerop (mod (+ (* 100 d1) (* 10 d2) d3) 5)))
                 do (push (list d1 d2 d3 d4 d5 d6 d7) t5s))
        finally (return t5s)))
        
(defun test-3 (t5s)
  (loop with t3s = nil
        for t5 in t5s
        for d8 = (seventh t5)
        for d7 = (sixth t5)
        for d6 = (fifth t5)
        for d5 = (fourth t5)
        for d4 = (third t5)
        for d3 = (second t5)
        for d2 = (first t5)
        do (loop for d1 from 0 to 9
                 if (and (/= d1 d2 d3 d4 d5 d6 d7 d8)
                         (zerop (mod (+ (* 100 d1) (* 10 d2) d3) 3)))
                 do (push (list d1 d2 d3 d4 d5 d6 d7 d8) t3s))
        finally (return t3s)))

(defun test-2 (t3s)
  (loop with t2s = nil
        for t3 in t3s
        for d9 = (eighth t3)
        for d8 = (seventh t3)
        for d7 = (sixth t3)
        for d6 = (fifth t3)
        for d5 = (fourth t3)
        for d4 = (third t3)
        for d3 = (second t3)
        for d2 = (first t3)
        do (loop for d1 from 0 to 9
                 if (and (/= d1 d2 d3 d4 d5 d6 d7 d8 d9)
                         (zerop (mod (+ (* 100 d1) (* 10 d2) d3) 2)))
                 do (push (list d1 d2 d3 d4 d5 d6 d7 d8 d9) t2s))
        finally (return t2s)))

(defun test-1 (t2s)
  (loop with t1s = nil
        for t2 in t2s
        for da = (ninth t2)
        for d9 = (eighth t2)
        for d8 = (seventh t2)
        for d7 = (sixth t2)
        for d6 = (fifth t2)
        for d5 = (fourth t2)
        for d4 = (third t2)
        for d3 = (second t2)
        for d2 = (first t2)
        do (loop for d1 from 0 to 9
                 if (/= d1 d2 d3 d4 d5 d6 d7 d8 d9 da)
                 do (push (list d1 d2 d3 d4 d5 d6 d7 d8 d9 da) t1s))
        finally (return t1s)))

(defun euler-043 ()
  (reduce #'+ (loop for i in (test-1 (test-2 (test-3 (test-5 (test-7 (test-11 (test-13 (test-17))))))))
                    collect (loop with n = 0
                                  for j in i
                                  do (setf n (+ (* 10 n) j))
                                  finally (return n)))))

;(time (euler-043))
