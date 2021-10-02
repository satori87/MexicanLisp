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
			game ) ; return game to go all the way back up to playRound
		( (null (hasValidMove game (getHumanHand game) validTrains) )
			game ) ; return game to go all the way back up to playRound
		( t
			;a successful move prompt returns a (game tile)
			(let* ( 
				(moveResult (promptForMove game validTrains) )
				(alteredGame (first moveResult) )
				(tilePlayed (first (rest moveResult) ) ) )
				(cond
					( (isDouble tilePlayed) ;if its a double, continue to play
						(makeHumanMoves alteredGame validTrains (+ tilesPlayed 1) ) )
					( t  ; otherwise just ruin the altered game object
						alteredGame )
				)				
			)
	)
)

(defun promptForMove (game validTrains)
	(let (
		(trainInput (getValidTrainInput validTrains) )
		(tile (getValidTileInput (getPlayerHand game) ) ) )
		(cond
			( (canPlayTileToTrain (getTrain game train) tile)
				(playTileToTrain game trainInput tile) )
			( t
				(promptForMove game validTrains) )
		)
	)
)

(defun canPlayTileToTrain (train tile)

)

(defun playTileToTrain (game trainNumber tile)

)

;return 1 2 3 for C H M
; so long as that nth is t
(defun getValidTrainInput (validTrains)

)

;Return an actual tile
(defun getValidTileInput (hand)

)