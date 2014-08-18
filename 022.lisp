
;;; What is the total of all the name scores in the file?
;;
;;  Using names.txt, a 46K text file containing over five-thousand
;;  first names, begin by sorting it into alphabetical order. Then
;;  working out the alphabetical value for each name, multiply this
;;  value by its alphabetical position in the list to obtain a name
;;  score.
;;
;;  For example, when the list is sorted into alphabetical order,
;;  COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name
;;  in the list. So, COLIN would obtain a score of 938 × 53 = 49714.

(in-package #:pe)

(defun read-file (file)
  (with-open-file (s file)
    (do ((l (read-line s) (read-line s nil 'eof))
	 lines)
	((eq l 'eof) lines)
      (setf lines (concatenate 'string lines l)))))

(defun split (names)
  (loop with start = 0
        for c from 0 below (length names)
        if (char= (char names c) #\,)
           collect (subseq names (1+ start) (1- c)) into namelist
           and do (setf start (1+ c))
        finally (return (append namelist (list (subseq names (1+ start) (1- c)))))))

(defun read-names (file)
  (split (read-file file)))

(defun letter-score (name)
  (reduce #'+ (map 'list
		   (lambda (c) (- (char-code c) (1- (char-code #\A))))
		   name)))

(defun score (names)
  (loop for n in names
        for i = 1 then (1+ i)
        sum (* i (letter-score n))))

(defun euler-022 ()
  (score (sort (read-names "files/names.txt") #'string<)))

;(time (euler-022))
