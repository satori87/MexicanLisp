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
			(endTurn game tilesPlayed) ) ; return game to go all the way back up to playRound
		( (null (hasValidMove game (getHumanHand game) validTrains) )
			(humanPass game validTrains tilesPlayed) ) ; return game to go all the way back up to playRound
		( t
			(makeHumanMove game validTrains tilesPlayed) )
	)
)

(defun remTile (hand tile)
	(remlist (remList hand tile) (reverseList tile) )
)

(defun makeHumanMove (game validTrains tilesPlayed)
	(let* ( 
		(moveResult (promptForMove game validTrains) )
		(tilePlayed (getNth 2 moveResult ) )
		(alteredGame (setHumanHand (first moveResult) (remTile (getHumanHand (first moveResult) ) tilePlayed ) ) ) )
		(cond
			( (isDouble tilePlayed) ;if its a double, continue to play
				(printRound alteredGame)
				(makeHumanMoves alteredGame validTrains (+ tilesPlayed 1) ) )
			( t  ; otherwise just end with the altered game object
				(endTurn alteredGame (+ tilesPlayed 1) ) )
		)		
	)
)

(defun humanPass (game validTrains tilesPlayed)
	;announce
	;if tilesplayed is 0 draw
	;if drawn card, try to play
	; if tilesplayed is still 0, set player passed on the 11th element
	;in the end return the altered game object
	; via endturn
	(princ "Human passes turn.") (terpri)
	(cond
		( (= tilesPlayed 0)
			(humanDraw game validTrains) )
		( t
			(endTurn game tilesPlayed) )
	)
)

(defun humanDraw (game validTrains)
	(let ( (boneyard (getBoneyard game) ) )
		(cond
			( (null boneyard)
				(endTurn game 0) )
			( t
				(checkDraw (setBoneyard (setHumanHand game (addTileToHand 'HUMAN (getHumanHand game) boneyard ) ) (rest boneyard ) ) validTrains) )
		)
	)
)

(defun checkDraw (game validTrains)
	;simply check if the card we drew is playable. if so, re-enter the turn loop
	(cond
		( (hasValidMove game (getHumanHand game) validTrains)
			(makeHumanMove game validTrains 0) )
		( t
			(endTurn game 0) )
	)
)

(defun promptForMove (game validTrains)
	(let (
			(trainNumber (getValidTrainInput game validTrains) )
			(tile (getLegalTileInput (getHumanHand game) ) )
		)
		(cond
			( (and (numberp tile) (= tile 1) )
				(promptForMove game validTrains) )
			( (canPlayTileToTrain game (getTrain game trainNumber) tile )
				(playTileToTrain game trainNumber tile 2) )
			( t
				(promptForMove game validTrains) )
		)
	)
)

;return 1 2 3 for C H M
; so long as that nth is t
(defun getValidTrainInput (game validTrains)
	(let ( (input (getValidNumber 1 4 "Enter a number for (1) Computer Train (2) Human Train or (3) Mexican Train or (4) for *HELP*") ) )
		(cond
			( (= input 4)
				(askForHelpgame validTrains)
				(getValidTrainInput validTrains) )
			( (getNth input validTrains)
				input )
			( t
				(princ "You may not play to that train!") (terpri)
				(getValidTrainInput validTrains) )
		)
	)
)

(defun askForHelp (game validTrains)
	
)

;Return an actual tile
(defun getLegalTileInput (hand)	
	(let ( (tile (getValidTileInput) ) )
		(cond
			( (and (numberp tile) (= tile 1))
				tile )
			( (listContains hand tile)
				tile )
			( (listContains hand (reverseList tile) )
				tile )
			( t
				(getLegalTileInput hand) )
		)
	)
)

; returns a valid formed tile
; e.g. list of 2 numbers 1-9
(defun getValidTileInput ()
	(princ "Enter a valid tile in your hand. Usage: (# #) or enter (1) to go back") (terpri)
	(let ( (tile (read) ) )
		(cond
			( (null tile)
				(getValidTileInput) )
			( (and (numberp tile) (= tile 1) )
				tile ) ;return a 1 if we wanna go back
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