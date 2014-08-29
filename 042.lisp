
;;; By converting each letter in a word to a number corresponding to
;;; its alphabetical position and adding these values we form a word
;;; value. For example, the word value for SKY is 19 + 11 + 25 = 55 =
;;; t_(10). If the word value is a triangle number then we shall call
;;; the word a triangle word.
;;;
;;; Using words.txt, a 16K text file containing nearly two-thousand
;;; common English words, how many are triangle words?

(in-package #:pe)

(defun words ()
  (with-open-file (s "files/words.txt")
    (read s)))

(defun squarep (n)
  (let ((r (floor (sqrt n))))
    (= n (* r r))))

(defun score-word (w)
  (reduce #'+ (map 'list (lambda (c) (- (char-code c) 64)) w)))

(defun triangularp (word)
  (squarep (1+ (* 8 (score-word word)))))

(defun euler-042 ()
  (loop for w in (words)
        when (triangularp w) count w))

;(time (euler-042))
