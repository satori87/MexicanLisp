;     ************************************************************
;     * Name:  Michael Whitlock                                  *
;     * Project:  Mexican Train (LISP)                           *
;     * Class:  OPL Fall 2021                                    *
;     * Date:  10/10/21                                          *
;     ************************************************************
;
;     human.lsp contains all functions unique to human player
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: takeHumanTurn
; Purpose: Prompt for option to save game or execute human turn
; Parameters:  game object
; Algorithm: Obtain valid number 1-2, 1 for save, 2 to take turn
; Return Value: the modified game object after turn is complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun takeHumanTurn (game)
	(cond
		( (equal (getValidNumber 1 2 "(1) Save (2) Start Turn") 1)
			(saveGame game) )
		( t
			(makeHumanMoves game (getValidTrains game (getValidHumanTrains game) ) 0 ) )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: getValidHumanTrains
; Purpose: Returns the list of trains eligible for human to player on (not accounting
;			for orphans)
; Parameters: game object
; Algorithm: own train and mexican train are always t, computer train is t if it has a marker
; Return Value: list of 3 booleans representring train validity
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun getValidHumanTrains (game)
	(list (hasMarker (getComputerTrain game) ) t t)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: makeHumanMoves
; Purpose: Executes recursively until human has played 3 tiles or has no moves. 
;			Then ends turn gracefully or passes turn
; Parameters: game object, valid trains, the number of tiles played this turn
; Algorithm: if 3 tiles have been played, triumphantly end turn with no room
;				for passing. if no valid moves are left, pass the turn
; Return Value: the modified game object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun makeHumanMoves (game validTrains tilesPlayed)
	(cond
		( (= tilesPlayed 3)
			(endTurn game tilesPlayed) ) ; return game to go all the way back up to playRound
		( (null (hasValidMove game (getHumanHand game) validTrains) )
			(humanPass game validTrains tilesPlayed) ) ; return game to go all the way back up to playRound
		( t
			(makeHumanMove game validTrains tilesPlayed) )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: makeHumanMove
; Purpose: Makes an individual move for the human. Only called when a legal move
;			exists. Guaranteed to find one
; Parameters: game object, valid trains, number of tiles played (so it can be passed 
;				recursively to makeHumanMoves)
; Algorithm: obtain a moveResult from promptForMove
;				extract the tile played and the new game state from the result
;				remove the played tile from hand
;				If tile is double, continue to play via recursion
;				Otherwise, end turn
; Return Value: the altered game state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: humanPass
; Purpose: Human passes turn because it has no valid move to make. Is not called if
;			3 tiles were played.
; Parameters: game object, valid trains, number tiles played
; Algorithm: If 0 tiles were played, attempt to draw a card. Otherwise, end turn
; Return Value: the altered game state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun humanPass (game validTrains tilesPlayed)
	(princ "Human passes turn.") (terpri)
	(cond
		( (= tilesPlayed 0)
			(humanDraw game validTrains) )
		( t
			(endTurn game tilesPlayed) )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: humanDraw
; Purpose: Attempts to draw a card from boneyard. If successful, attempts to play it
; Parameters: game object, valid trains
; Algorithm: If boneyard is empty, end turn and report 0 tiles played
;				Otherwise draw a card and immediately try to play it
; Return Value: the altered game state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: checkDraw
; Purpose: Called after human draws a tile for passing. Continues turn if a valid move
;			was created by drawing
; Parameters: game object, valid trains
; Algorithm: checks if the computer has a valid move, if so, makes it. otherwise, ends
;				turn and reports 0 tiles played
; Return Value: altered game state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun checkDraw (game validTrains)
	;simply check if the card we drew is playable. if so, re-enter the turn loop
	(cond
		( (hasValidMove game (getHumanHand game) validTrains)
			(makeHumanMove game validTrains 0) )
		( t
			(endTurn game 0) )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: promptForMove
; Purpose: prompt human for a valid move and execute it
; Parameters: game object, valid trains
; Algorithm: obtain valid train input
;			obtain valid tile input
;			if tile input is a 1 (they asked to go back), repeat recursively
;			if the specified tile can legally be played to the specified train, do so
;			otherwise, recursively repeat
; Return Value: the altered game object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: getValidTrainInput
; Purpose: prompts human to play on 1 2 or 3 train, or 4 to ask for help
; Parameters: game object, valid trains
; Algorithm: obtain a number 1-4 from console. If 4, display advice, otherwise
;				check train input against valid trains
;				if it checks out, return the input, otherwise recursively repeat
; Return Value: a valid number 1, 2, or 3 that is valid according to validTrains
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun getValidTrainInput (game validTrains)
	(let ( (input (getValidNumber 1 4 "Enter a number for (1) Computer Train (2) Human Train or (3) Mexican Train or (4) for *HELP*") ) )
		(cond
			( (= input 4)
				(askForHelp game validTrains)
				(getValidTrainInput game validTrains) )
			( (getNth input validTrains)
				input )
			( t
				(princ "You may not play to that train!") (terpri)
				(getValidTrainInput game validTrains) )
		)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: getLegalTileInput
; Purpose: gets a tile input from human that is not only a valid tile, it is also in
;				in their hand
; Parameters: a hand
; Algorithm: obtain a valid tile from console, look for it in hand
; Return Value: a tile contained in hand
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: getValidTileInput
; Purpose: Obtains a validly formed tile e.g. 2 numbers between 0 and 9. Must be a tile
;			in the 55 tile double 9 set
; Parameters:  N/A
; Algorithm: Obtain console input and validate it with a series of bound checks
; Return Value: a double 9 tile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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