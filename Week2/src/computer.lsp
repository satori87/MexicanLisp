;     ************************************************************
;     * Name:  Michael Whitlock                                  *
;     * Project:  Mexican Train (LISP)                           *
;     * Class:  OPL Fall 2021                                    *
;     * Date:  10/10/21                                          *
;     ************************************************************
;
;     computer.lsp contains all functions unique to computer player
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: takeComputerTurn
; Purpose: Prompt for option to save game or execute computer turn
; Parameters:  game object
; Algorithm: Obtain valid number 1-2, 1 for save, 2 to take turn
; Return Value: the modified game object after turn is complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun takeComputerTurn (game)
	(cond
		( (equal (getValidNumber 1 2 "(1) Save (2) Start Turn") 1)
			(saveGame game) )
		( t
			(makeComputerMoves game (getValidTrains game (getValidComputerTrains game) ) 0 ) )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: getValidComputerTrains
; Purpose: Returns the list of trains eligible for computer to player on (not accounting
;			for orphans)
; Parameters: game object
; Algorithm: own train and mexican train are always t, human train is t if it has a marker
; Return Value: list of 3 booleans representring train validity
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun getValidComputerTrains (game)
	(list t (hasMarker (getHumanTrain game) ) t )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: makeComputerMoves
; Purpose: Executes recursively until computer has played 3 tiles or has no moves. 
;			Then ends turn gracefully or passes turn
; Parameters: game object, valid trains, the number of tiles played this turn
; Algorithm: if 3 tiles have been played, triumphantly end turn with no room
;				for passing. if no valid moves are left, pass the turn
; Return Value: the modified game object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: makeComputerMove
; Purpose: Makes an individual move for the computer. Only called when a legal move
;			exists. Guaranteed to find one
; Parameters: game object, valid trains, number of tiles played (so it can be passed 
;				recursively to makeComputerMoves)
; Algorithm: obtain a moveResult from the AI
;				extract the tile played and the new game state from the result
;				remove the played tile from hand
;				If tile is double, continue to play via recursion
;				Otherwise, end turn
; Return Value: the altered game state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun makeComputerMove (game validTrains tilesPlayed)
	(let* ( 
		(moveResult (getComputerMove game validTrains) )
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: computerPass
; Purpose: Computer passes turn because it has no valid move to make. Is not called if
;			3 tiles were played.
; Parameters: game object, valid trains, number tiles played
; Algorithm: If 0 tiles were played, attempt to draw a card. Otherwise, end turn
; Return Value: the altered game state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: computerDraw
; Purpose: Attempts to draw a card from boneyard. If successful, attempts to play it
; Parameters: game object, valid trains
; Algorithm: If boneyard is empty, end turn and report 0 tiles played
;				Otherwise draw a card and immediately try to play it
; Return Value: the altered game state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: checkComputerDraw
; Purpose: Called after computer draws a tile for passing. Continues turn if a valid move
;			was created by drawing
; Parameters: game object, valid trains
; Algorithm: checks if the computer has a valid move, if so, makes it. otherwise, ends
;				turn and reports 0 tiles played
; Return Value: altered game state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun checkComputerDraw (game validTrains)
	;simply check if the card we drew is playable. if so, re-enter the turn loop
	(cond
		( (hasValidMove game (getComputerHand game) validTrains)
			(makeComputerMove game validTrains 0) )
		( t
			(endTurn game 0) )
	)
)