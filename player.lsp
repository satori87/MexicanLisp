(defun endTurn (game tilesPlayed)
	(cond
		( (equal (getNextPlayer game) 'Computer)
			(princ "***Computer Ends Turn. Human Starts Turn***") (terpri)
			(setComputerTrain (setComputerPassed (setNextPlayer game 'Human) (= tilesPlayed 0) ) (setMarker (getComputerTrain game) (= tilesPlayed 0) ) ) )
		( (equal (getNextPlayer game) 'Human)
			(format t "***Human Ends Turn. Computer Starts Turn***  Human played ~d tiles" tilesPlayed) (terpri)
			(setHumanTrain (setHumanPassed (setNextPlayer game 'Computer) (= tilesPlayed 0) ) (setMarker (getHumanTrain game) (= tilesPlayed 0) ) ) )
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