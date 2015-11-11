;;; Convergents of e
;;
;; ...
;;
;; What is most surprising is that the important mathematical constant,
;; e = [2; 1,2,1, 1,4,1, 1,6,1 , ... , 1,2k,1, ...].
;; 
;; The first ten terms in the sequence of convergents for e are:
;; 
;; 2, 3, 8/3, 11/4, 19/7, 87/32, 106/39, 193/71, 1264/465, 1457/536, ...
;; 
;; The sum of digits in the numerator of the 10th convergent is 1+4+5+7=17.
;; 
;; Find the sum of digits in the numerator of the 100th convergent of
;; the continued fraction for e.

(in-package #:pe)

(defun convergent (begin end)
  (cond
    ((= begin end)
     0)
    ((zerop begin)
     (+ 2 (convergent (+ 1 begin) end)))
    ((= 2 (mod begin 3))
     (/ 1 (+ (* 2 (/ (+ 1 begin) 3))
	     (convergent (+ 1 begin) end))))
    (t
     (/ 1 (+ 1 (convergent (+ 1 begin) end))))))

(defun sum-digits-str (str)
  (if (zerop (length str))
      0
    (+ (- (char-code (elt str 0)) 48)
       (sum-digits-str (subseq str 1)))))

(defun euler-065 ()
  (sum-digits-str
   (format nil "~A" (numerator (convergent 0 100)))))

