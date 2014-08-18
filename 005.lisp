
(defun euler-005 ()
  (lcm 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))

;(loop with e = (euler-005)
;      for i in '(2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)
;      do (format t "~&(eql (mod ~d ~d) ~d)~%" e i (mod e i)))

;(euler-005)