(defun takeComputerTurn (game)
	(cond
		( (equal (getValidNumber 1 2 "(1) Save (2) Start Turn") 1)
			(saveGame game) )
		( t
			(playComputerTurn game (getValidTrains game (getValidComputerTrains game) ) ) )
	)
)

(defun getValidComputerTrains (game)
	(list t t (hasMarker (getHumanTrain game) ) )
)

(defun playComputerTurn (game validTrains)
	;valid trains represents the valid trains to play on
	;this turn. for the entire rest of turn these are 
	(makeComputerMoves game validTrains 0)
)

(defun makeComputerMoves (game validTrains tilesPlayed)
	(cond
		( (= tilesPlayed 3)
			(endTurn game tilesPlayed) ) ; return game to go all the way back up to playRound
		( (null (hasValidMove game (getComputerHand game) validTrains) )
			(computerPass game validTrains tilesPlayed) ) ; return game to go all the way back up to playRound
		( t
			(makeComputerMove game validTrains tilesPlayed) )
	)
)

(defun makeComputerMove (game validTrains tilesPlayed)
	(let* ( 
		(moveResult (promptForMove game validTrains) )
		(tilePlayed (getNth 2 moveResult ) )
		(alteredGame (setComputerHand (first moveResult) (remTile (getComputerHand (first moveResult) ) tilePlayed ) ) ) )
		(cond
			( (isDouble tilePlayed) ;if its a double, continue to play
				(printRound alteredGame)
				(makeComputerMoves alteredGame validTrains (+ tilesPlayed 1) ) )
			( t  ; otherwise just end with the altered game object
				(endTurn alteredGame (+ tilesPlayed 1) ) )
		)		
	)
)

(defun computerPass (game validTrains tilesPlayed)
	;announce
	;if tilesplayed is 0 draw
	;if drawn card, try to play
	; if tilesplayed is still 0, set player passed on the 11th element
	;in the end return the altered game object
	; via endturn
	(princ "Computer passes turn.") (terpri)
	(cond
		( (= tilesPlayed 0)
			(computerDraw game validTrains) )
		( t
			(endTurn game tilesPlayed) )
	)
)

(defun computerDraw (game validTrains)
	(let ( (boneyard (getBoneyard game) ) )
		(cond
			( (null boneyard)
				(endTurn game 0) )
			( t
				(checkComputerDraw (setBoneyard (setComputerHand game (addTileToHand 'COMPUTER (getComputerHand game) boneyard ) ) (rest boneyard ) ) validTrains) )
		)
	)
)

(defun checkComputerDraw (game validTrains)
	;simply check if the card we drew is playable. if so, re-enter the turn loop
	(cond
		( (hasValidMove game (getComputerHand game) validTrains)
			(makeComputerMove game validTrains 0) )
		( t
			(endTurn game 0) )
	)
)