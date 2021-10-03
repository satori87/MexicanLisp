(defun endTurn (game)
	(cond
		( (equal (getNextPlayer game) 'Computer)
			(princ "***Computer Ends Turn. Human Starts Turn***")
			(terpri)
			(setNextPlayer game 'Human) )
		( (equal (getNextPlayer game) 'Human)
			(princ "***Human Ends Turn. Computer Starts Turn***")
			(terpri)
			(setNextPlayer game 'Computer) )
	)
)

(defun tallyHand (hand)
	(cond 
		( (null hand)
			0 )
		( t
			(+ (getTileValue (first hand) ) (tallyHand (rest hand) ) ) )
	)
)

(defun getValidTrains (game playableTrains)
	(cond
		( (isAnyOrphans game)
			(getOrphanTrains game) )
		( t
			playableTrains )
	)
)

(defun hasValidMove (game hand validTrains)
	(princ "hey2") (terpri)
	(cond
		( (null hand)
			() )
		( t
			(or (canPlayTileAnywhere game (first hand) validTrains) (hasValidMove game (rest hand) validTrains) ) )
	)
)

(defun canPlayTileAnywhere (game tile validTrains)
	(princ "hey") (terpri)
	(cond
		( (and (getNth 1 validTrains) (canPlayTileToTrain game (getComputerTrain game) tile) )
			t )
		( (and (getNth 2 validTrains) (canPlayTileToTrain game (getHumanTrain game) tile) )
			t )
		( (and (getNth 3 validTrains) (canPlayTileToTrain game (getMexicanTrain game) tile) )
			t )
		( t
			() )
	)
)