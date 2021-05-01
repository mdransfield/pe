;; By replacing each of the letters in the word CARE with 1, 2, 9, and
;; 6 respectively, we form a square number: 1296 = 362. What is
;; remarkable is that, by using the same digital substitutions, the
;; anagram, RACE, also forms a square number: 9216 = 962. We shall
;; call CARE (and RACE) a square anagram word pair and specify further
;; that leading zeroes are not permitted, neither may a different
;; letter have the same digital value as another letter.
;;
;; Using words.txt (right click and 'Save Link/Target As...'), a 16K
;; text file containing nearly two-thousand common English words, find
;; all the square anagram word pairs (a palindromic word is NOT
;; considered to be an anagram of itself).
;;
;; What is the largest square number formed by any member of such a pair?
;;
;; NOTE: All anagrams formed must be contained in the given text file.

(in-package :PE)

(defparameter *wordlist-filepath* (merge-pathnames #P"files/p098_words.txt"))

(defun read-words (filepath)
  "Read words from the file at FILEPATH putting them into a table,
which is returned."
  (loop with table = (make-hash-table :test 'equal)
	with word
	for quoted-word in (cl-utilities:split-sequence #\, (with-open-file (instream filepath) (read-line instream)))
	do (setf word (string-trim '(#\") quoted-word))
	   (push word (gethash (sort (copy-seq word) #'char<) table))
	finally (return (loop for key being each hash-key in table
				using (hash-value value)
			      when (= 1 (length value))
				do (remhash key table)
			      finally (return table)))))

(defun arrange-words (table)
  "Generate a hash-table of word anagram sets keyed by the length of the words."
  (loop with new-tbl = (make-hash-table)
	for k being the hash-keys of table
	  using (hash-value v)
	do (push v (gethash (length k) new-tbl))
	finally (return new-tbl)))

(defun number-from (arr)
  "Make a number from the sequence of digits in ARR."
  (when (listp arr)
    (setf arr (make-array (length arr) :initial-contents arr)))
  (loop with x = 0
	  for d across arr
	  do (setf x (+ (* x 10) d))
	  finally (return x)))

(defun make-squares ()
  "Make a table of square number groups keyed by the number of digits."
  (loop with table = (make-hash-table)
	with sq
	for k from 31621 downto 4
	do (setf sq (expt k 2))
	   (push sq (gethash (floor (1+ (log sq 10))) table))
	finally (return table)))

(defun make-word-map (word num)
  "Make a map of the digits of NUM keyed by the letters of WORD.
Return NIL if two different letters would map to the same value."
  (loop with dmap = (make-hash-table)
	with cmap = (make-hash-table)
	with dgs = (make-array (length word) :initial-contents (digits-of num))
	for i below (length word)
	if (or (and (gethash (aref word i) dmap) (/= (gethash (aref word i) dmap) (aref dgs i)))
	       (and (gethash (aref dgs i) cmap)  (char/= (gethash (aref dgs i) cmap) (aref word i))))
	  return nil
	do (setf (gethash (aref word i) dmap) (aref dgs i)
		 (gethash (aref dgs i) cmap) (aref word i))
	finally (return dmap)))

(defun map-word-to-number (map word)
  "Convert WORD to a number using MAP."
  (number-from (loop for l across word collect (gethash l map))))

(defun max-square-for-anagram (w1 w2 nums)
  "The maximum square of anagrams W1 and W2 from NUMS."
  (loop with map
	for num in nums
	do (setf map (make-word-map w1 num))
	when (and map (position (map-word-to-number map w2) nums))
	  maximize (max num (map-word-to-number map w2))))

(defun max-square-for-anagram-set (anagrams nums)
  "The maximum square for a set of ANAGRAMS from NUMS."
  (loop for x on anagrams
	  when (> (length x) 1)
	    maximize (loop for y in (cdr x)
			   maximize (max-square-for-anagram (car x) y nums))))

(defun max-square-of-length (len words squares)
  "The maximum square from a set of WORDS of length LEN."
  (loop for set in (gethash len words)
	maximize (max-square-for-anagram-set set (gethash len squares))))

(defun debug-word-map (map)
  "Return a list of pairs of (key value) of the entries in MAP."
  (loop for k being the hash-keys of map
	  using (hash-value v)
	collect (cons k v)))

(defun euler-098 ()
  (loop with words = (arrange-words (read-words *wordlist-filepath*))
	and squares = (make-squares)
	for len being the hash-keys of words
	maximize (max-square-of-length len words squares)))
