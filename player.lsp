(defun endTurn (game)
	(cond
		( (equal (getNextPlayer game) "COMPUTER" )
			(setNextPlayer game "HUMAN") )
		( (equal (getNextPlayer game) "HUMAN" )
			(setNextPlayer game "COMPUTER") )
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