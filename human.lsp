(defun takeHumanTurn (game)
	(cond
		( (equal (getValidNumber 1 2 "(1) Save (2) Start Turn") 1)
			(saveGame game) )
		( t
			(playHumanTurn game (getValidTrains game (getValidHumanTrains game) ) ) )
	)
)

(defun getValidHumanTrains (game)
	(list (hasMarker (getComputerTrain game) ) t t)
)

(defun playHumanTurn (game validTrains)
	;valid trains represents the valid trains to play on
	;this turn. for the entire rest of turn these are 
	(makeHumanMoves game validTrains 0)
)

(defun makeHumanMoves (game validTrains tilesPlayed)
	(cond
		( (= tilesPlayed 3)
			() )
		( (null (hasValidMove game (getHumanHand game) validTrains) )
			() )
		( t
			(makeHumanMoves (promptForMove game validTrains) validTrains (+ tilesPlayed 1) ) )
	)
)

(defun promptforMove (game validTrains)
	;obtain input
	;validate input
	;validate legality of dest train
	;validate legality of move
	;make move and return altered game
)