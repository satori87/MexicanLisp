(defun endTurn (game)
	(cond
		( (string= (getNextPlayer game) 'Computer)
			(princ "***Computer Ends Turn. Human Starts Turn***")
			(terpri)
			(setNextPlayer game 'Human) )
		( (string= (getNextPlayer game) 'Human)
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
			(andList playableTrains (getOrphanTrains game) ) )
		( t
			playableTrains )
	)
)

(defun hasValidMove (game hand validTrains)

)