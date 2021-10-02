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
			(endTurn game) ) ; return game to go all the way back up to playRound
		( (null (hasValidMove game (getHumanHand game) validTrains) )
			(humanPass game tilesPlayed) ) ; return game to go all the way back up to playRound
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
)

(defun humanPass (game tilesPlayed)
	;announce
	;if tilesplayed is 0 draw
	;if drawn card, try to play
	; if tilesplayed is still 0, set player passed on the 11th element
	;in the end return the altered game object
	; via endturn
	(endTurn game)
)

(defun promptForMove (game validTrains)
	(let (
			(train (getValidTrainInput validTrains) )
			(tile (getValidTileInput (getPlayerHand game) ) )
		)
		(cond
			( (canPlayTileToTrain game (getTrain game train) tile)
				(playTileToTrain game train tile) )
			( t
				(promptForMove game validTrains) )
		)
	)
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