;; In the card game poker, a hand consists of five cards and are
;; ranked, from lowest to highest, in the following way:
;;
;; * High Card: Highest value card.
;; * One Pair: Two cards of the same value.
;; * Two Pairs: Two different pairs.
;; * Three of a Kind: Three cards of the same value.
;; * Straight: All cards are consecutive values.
;; * Flush: All cards of the same suit.
;; * Full House: Three of a kind and a pair.
;; * Four of a Kind: Four cards of the same value.
;; * Straight Flush: All cards are consecutive values of same suit.
;; * Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.
;;
;; The cards are valued in the order:
;; 2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.
;; 
;; If two players have the same ranked hands then the rank made up of
;; the highest value wins; for example, a pair of eights beats a pair
;; of fives (see example 1 below).  But if two ranks tie, for example,
;; both players have a pair of queens, then highest cards in each hand
;; are compared (see example 4 below); if the highest cards tie then
;; the next highest cards are compared, and so on.
;; 
;; Consider the following five hands dealt to two players:
;; Hand	Player 1	 	Player 2	 	Winner
;; 1	5H 5C 6S 7S KD		2C 3S 8S 8D TD		Player 2
;;	Pair of Fives		Pair of Eights
;;
;; 2	5D 8C 9S JS AC		2C 5C 7D 8S QH		Player 1
;; 	Highest card Ace	Highest card Queen
;;
;; 3	2D 9C AS AH AC		3D 6D 7D TD QD		Player 2
;; 	Three Aces		Flush with Diamonds
;;
;; 4	4D 6S 9H QH QC		3D 6D 7H QD QS		Player 1
;; 	Pair of Queens		Pair of Queens
;; 	Highest card Nine	Highest card Seven
;;
;; 5	2H 2D 4C 4D 4S		3C 3D 3S 9S 9D		Player 1
;;	Full House		Full House
;;	With Three Fours	with Three Threes
;;
;; The file, poker.txt, contains one-thousand random hands dealt to
;; two players.  Each line of the file contains ten cards (separated
;; by a single space): the first five are Player 1's cards and the
;; last five are Player 2's cards.  You can assume that all hands are
;; valid (no invalid characters or repeated cards), each player's hand
;; is in no specific order, and in each hand there is a clear winner.
;;
;; How many hands does Player 1 win?

(in-package #:pe)

(defconstant clubs 0)
(defconstant diamonds 1)
(defconstant hearts 2)
(defconstant spades 3)

(defconstant ace 14)
(defconstant king 13)
(defconstant queen 12)
(defconstant jack 11)

(defconstant royal-flush 9)
(defconstant straight-flush 8)
(defconstant four-of-a-kind 7)
(defconstant full-house 6)
(defconstant flush 5)
(defconstant straight 4)
(defconstant three-of-a-kind 3)
(defconstant two-pairs 2)
(defconstant one-pair 1)
(defconstant high-card 0)

(defun tokenize (str chr)
  (loop for i = 0 then (1+ j)
        for j = (position chr str :start i)
        collect (if j (subseq str i j)
		    (subseq str i))
        while j))

(defun char-suit (char)
  (case char
    (#\C clubs)
    (#\D diamonds)
    (#\H hearts)
    (#\S spades)))

(defun char-rank (char)
  (case char
    (#\A ace)
    (#\K king)
    (#\Q queen)
    (#\J jack)
    (#\T 10)
    ((#\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2)
     (- (char-code char) (char-code #\0)))))

(defun convert-card (card)
  (let ((rank (char-rank (char card 0)))
	(suit (char-suit (char card 1))))
    (+ (* suit 15) rank)))

(defun rank (card)
  (mod card 15))

(defun suit (card)
  (floor card 15))

(defun sort-hand (hand)
  (sort hand #'> :key #'rank))

(defun convert (cards)
  (sort-hand (mapcar #'convert-card cards)))

(defun parse (line)
  (let ((raw-cards (tokenize line #\Space)))
    (list (convert (subseq raw-cards 0 5))
	  (convert (subseq raw-cards 5)))))

(defun ranks (hand)
  (mapcar #'rank hand))

(defun suits (hand)
  (mapcar #'suit hand))

(defun successive-differences (ranks)
  (butlast (maplist (lambda (x)
		      (when (cadr x)
			(- (first x) (second x)))) 
		    ranks)))

(defun positions-of (x seq)
  (loop for i from 0 below (length seq)
        when (= x (elt seq i))
        collect i))

(defun high-card-p (hand)
  (list high-card hand))

(defun old-high-card-p (hand)
  (list high-card (first hand) (rest hand)))

(defun one-pair-p (hand)
  (let* ((diffs (successive-differences (ranks hand)))
	 (zeros (positions-of 0 diffs)))
    (and (= 1 (length zeros))
	 (let* ((pair (rank (elt hand (position 0 diffs))))
		(remnant (remove-if (lambda (x) (= pair (rank x))) hand)))
	   (list one-pair pair remnant)))))

(defun two-pairs-p (hand)
  (let* ((diffs (successive-differences (ranks hand)))
	 (zeros (positions-of 0 diffs)))
    (and (= (length zeros) 2)
	 (/= 1 (- (second zeros) (first zeros)))
	 (let* ((pair1 (rank (elt hand (first zeros))))
		(pair2 (rank (elt hand (second zeros))))
		(remnant (remove-if (lambda (x)
				      (let ((rank-x (rank x)))
					(or (= pair1 rank-x)
					    (= pair2 rank-x)))) hand)))
	   (list two-pairs pair1 pair2 remnant)))))

(defun three-of-a-kind-p (hand)
  (let* ((diffs (successive-differences (ranks hand)))
	 (zeros (positions-of 0 diffs)))
    (and (= (length zeros) 2)
	 (= 1 (- (second zeros) (first zeros)))
	 (let* ((trip-rank (rank (elt hand (first zeros))))
		(remnant (remove-if (lambda (x) (= trip-rank (rank x))) hand)))
	   (list three-of-a-kind trip-rank remnant)))))

(defun straightp (hand)
  (let ((diffs (successive-differences (ranks hand))))
    (and (apply #'= diffs)
	 (= 1 (first diffs))
	 (list straight (first hand) hand))))

(defun flushp (hand)
  (and (apply #'= (suits hand))
       (list flush (rank (first (sort-hand hand))) hand)))

(defun full-house-p (hand)
  (let* ((diffs (successive-differences (ranks hand)))
	 (zeros (positions-of 0 diffs)))
    (and (= 3 (length zeros))
	 (or (and (= 0 (first diffs) (second diffs) (fourth diffs))
		  (list full-house
			(rank (elt hand 0))
			(rank (elt hand 4))))
	     (and (= 0 (first diffs) (third diffs) (fourth diffs))
		  (list full-house
			(rank (elt hand 4))
			(rank (elt hand 0))))))))

(defun four-of-a-kind-p (hand)
  (let* ((diffs (successive-differences (ranks hand)))
	 (zeros (positions-of 0 diffs)))
    (and (= 3 (length zeros))
	 (let* ((four-rank (rank (elt hand (first zeros))))
		(remnant (remove-if (lambda (x) (= four-rank (rank x))) hand)))
	   (list four-of-a-kind four-rank remnant)))))

(defun straight-flush-p (hand)
  (and (flushp hand)
       (straightp hand)
       (list straight-flush hand)))

(defun royal-flush-p (hand)
  (and (straight-flush-p hand)
       (= ace (rank (first hand)))
       (list royal-flush hand)))

(defun score-hand (hand)
  (or
   (royal-flush-p hand)
   (straight-flush-p hand)
   (four-of-a-kind-p hand)
   (full-house-p hand)
   (flushp hand)
   (straightp hand)
   (three-of-a-kind-p hand)
   (two-pairs-p hand)
   (one-pair-p hand)
   (high-card-p hand)))

(defun score (hands)
  (loop for hand in hands
        collect (score-hand hand)))

(defun decide-high-card (hand1 hand2)
  (loop 
     for i from 0 below (length hand1)
     for card1 = (rank (elt hand1 i))
     for card2 = (rank (elt hand2 i))
     for winner = (signum (- card1 card2))
     if (not (zerop winner)) return winner
     finally (return 'split)))

(defun decide-split (split-score hand1 hand2)
  (case split-score
    (0 (decide-high-card (second hand1) (second hand2)))
    (1 (let ((winner (signum (- (second hand1) (second hand2)))))
	 (if (not (zerop winner))
	     winner
	     (decide-high-card (third hand1) (third hand2)))))))

(defun decide (hands)
  (let* ((hand1 (first hands))
	 (hand2 (second hands))
	 (score1 (first hand1))
	 (score2 (first hand2))
	 (diff (signum (- score1 score2))))
    (if (not (zerop diff))
	diff
	(decide-split score1 hand1 hand2))))

(defun read-hands (file)
  (with-open-file (inp file)
    (loop for line = (read-line inp nil)
	  while line
	  count (= 1 (decide (score (parse line)))))))

(defun euler-054 ()
  (read-hands "files/poker.txt"))

;(time (euler-054))

