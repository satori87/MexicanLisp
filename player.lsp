(defun endTurn (game)
	(cond
		( (equal (getNextPlayer game) "Computer" )
			() )
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