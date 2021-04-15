
(defparameter *wordlist-filepath* (merge-pathnames #P"files/p098_words.txt"))

;; I wonder what happens if we filter out all squares which don't form anagram pairs?
(defun make-squares ()
  (loop with table = (make-hash-table)
	with square
	for k from 4 below 31623
	do (setf square (expt k 2))
	   (push square (gethash (floor (1+ (log square 10))) table))
	finally (return table)))

(defun read-words (filename)
  (loop with table = (make-hash-table :test 'equal)
	with word
	for quoted-word in (cl-utilities:split-sequence #\, (with-open-file (instream filename) (read-line instream)))
	do (setf word (string-trim '(#\") quoted-word))
	   (push word (gethash (sort (copy-seq word) #'char<) table))
	finally (return (loop for key being each hash-key in table
				using (hash-value value)
			      when (= 1 (length value))
				do (remhash key table)
			      finally (return table)))))

#|
(loop with squares = (gethash 3 squares)
      with square-anagrams and dgts
      for square in squares
      do (loop 
|#
