(defun newBoneyard()
	;create list of numbers 1 through 55 in random order, use to shuffle boneyard into new collection
	(shuffledBoneyard 0 (getRandomOrder) () )
)

(defun shuffledBoneyard(n order boneyard)
	(cond
		( (>= n 55)
			boneyard )
		( t
			(shuffledBoneyard (+ n 1) order (cons (getNth (getNth n order) (rawBoneyard) ) boneyard) ) )
	)
)

(defun getRandomOrder()
	(randomOrder () )
)

;return a list of the first 55 numbers in random order
(defun randomOrder(order)
	(cond
		( (= (getListLength order) 55)
			order )
		( t 
			(randomOrder (cons (getUnusedRandomNumber 0 order) order) ) )
	)
)

;return a number 1-55 not inside list
(defun getUnusedRandomNumber(n order)
	(let( ( rand (random 55) ) )
		(cond
			( (null (listContains order rand) )
				rand )
			( t
				(getUnusedRandomNumber n order) )
		)
	)
)

(defun rawBoneyard ()
	;Full 55-tile double-9 set
	'(  (0 0) (0 1) (0 2) (0 3) (0 4) (0 5) (0 6) (0 7) (0 8) (0 9) (1 1)
		(1 2) (1 3) (1 4) (1 5) (1 6) (1 7) (1 8) (1 9) (2 2) (2 3) (2 4)
		(2 5) (2 6) (2 7) (2 8) (2 9) (3 3) (3 4) (3 5) (3 6) (3 7) (3 8)
		(3 9) (4 4) (4 5) (4 6) (4 7) (4 8) (4 9) (5 5) (5 6) (5 7) (5 8)
		(5 9) (6 6) (6 7) (6 8) (6 9) (7 7) (7 8) (7 9) (8 8) (8 9) (9 9) )
)


(defun addTileToHand(name hand boneyard)
	(format t "~d drew a ~d from the boneyard" name (first boneyard) ) (terpri)
	(append hand (list (first boneyard)) )
)

(defun removeFromBoneyard(boneyard)
	(rest boneyard)
)