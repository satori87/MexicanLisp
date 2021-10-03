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
	(printlistLn '"Playable Trains: " validTrains )
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
						(printRound alteredGame)
						(makeHumanMoves alteredGame validTrains (+ tilesPlayed 1) ) )
					( t  ; otherwise just ruin the altered game object
						(endTurn alteredGame) )
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
			(trainNumber (getValidTrainInput validTrains) )
			(tile (getLegalTileInput (getHumanHand game) ) )
		)
		(cond
			( (canPlayTileToTrain game (getTrain game trainNumber) tile )
				(playTileToTrain game trainNumber tile ) )
			( t
				(promptForMove game validTrains) )
		)
	)
)

;return 1 2 3 for C H M
; so long as that nth is t
(defun getValidTrainInput (validTrains)
	(let ( (input (getValidNumber 1 3 "Enter a number for (1) Computer Train (2) Human Train or (3) Mexican Train") ) )
		(cond
			( (getNth input validTrains)
				input )
			( t
				(princ "You may not play to that train!") (terpri)
				(getValidTrainInput validTrains) )
		)
	)
)

;Return an actual tile
(defun getLegalTileInput (hand)	
	(let ( (tile (getValidTileInput) ) )
		(cond
			( (listContains hand tile)
				tile )
			( (listContains hand (reverseList (tile) ) )
				tile )
			( t
				(getLegalTileInput hand) )
		)
	)
)

; returns a valid formed tile
; e.g. list of 2 numbers 1-9
(defun getValidTileInput ()
	(princ "Enter a valid tile in your hand. Enter it by typing the first tile number, then a space, then the second number. Then press Enter: ") (terpri)
	(let ( (tile (read) ) )
		(cond
			( (null tile)
				(getValidTileInput) )
			( (equal (listp tile) () )
				(getValidTileInput) )
			( (/= (getListLength tile) 2)
				(getValidTileInput) )
			( (equal (numberp (first tile) ) () )
				(getValidTileInput) )
			( (equal (numberp (first (rest tile) ) ) () )
				(getValidTileInput) )
			( (< (first tile) 0)
				(getValidTileInput) )
			( (> (first tile) 9)
				(getValidTileInput) )
			( (< (first (rest tile) ) 0)
				(getValidTileInput) )
			( (> (first (rest tile) ) 9)
				(getValidTileInput) )
			( t
				tile )
		)
	)
)

















