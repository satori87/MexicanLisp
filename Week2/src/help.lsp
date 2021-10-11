;     ************************************************************
;     * Name:  Michael Whitlock                                  *
;     * Project:  Mexican Train (LISP)                           *
;     * Class:  OPL Fall 2021                                    *
;     * Date:  10/10/21                                          *
;     ************************************************************
;
;     help.lsp encompasses all of the modular functions related to
;		AI choices for moving. Entry points are askForHelp and
;		getComputerMove
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: askForHelp
; Purpose: Serves as Human's entry point to the AI system. Prints best move for human player
; Parameters: game object, list of valid trains to play on
; Algorithm: Check if human has a valid move. If so, calculate and display it.
; Return Value: N/A
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun askForHelp (game validTrains)
	(cond
		( (null (hasValidMove game (getHumanHand game) validTrains) )
			(princ "You have no valid move.") (terpri) )
		( t
			(let* (
				(advice (getBestMove game validTrains 2) )
				(trainNumber (getNth 1 advice ) )
				(tile (getNth 2 advice ) )
				)
				(format t '"I recommend you play ~d to ~d because ~d " tile (getTrainName trainNumber) (getMoveReason game trainNumber tile 2) ) (terpri)
			)
		)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: getComputerMove
; Purpose: Serves as Computer's entry point to the AI system. Prints and executes best
;			move for computer player
; Parameters: game object, list of valid trains
; Algorithm: Check if computer has a valid move. If so, calculate, execute, and display it.
; Return Value: the finalized play
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun getComputerMove (game validTrains)
	(cond
		( (null (hasValidMove game (getComputerHand game) validTrains) )
			(princ "Computer has no valid move.") (terpri) )
		( t
			(let* (
				(advice (getBestMove game validTrains 1) )
				(trainNumber (getNth 1 advice ) )
				(tile (getNth 2 advice ) )
				)
				(format t '"Computer plays ~d to ~d because ~d " tile (getTrainName trainNumber) (getMoveReason game trainNumber tile 1) ) (terpri)
				(playTileToTrain game trainNumber tile 1)
			)
		)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: getMoveReason
; Purpose: Produces text explanations of play justifications and displays them
; Parameters: the game object, train number played to, tile played, player number who played
; Algorithm: If tile is double, reason was that. If player has a marker and plays to own train
;				the reason is that. Otherwise reason is because that was highest pip count tile
; Return Value: N/A
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun getMoveReason (game trainNumber tile playerNumber)
	(cond
		( (isDouble tile)
			(format nil '"~d is the highest playable double, allowing another turn" tile) )
		( (and (= trainNumber playerNumber) (playerHasMarker game playerNumber) )
			(format nil '"~d is the best tile playable to own train, which has a marker to consider" tile) )
		( t
			(format nil '"~d is the highest pip count tile in hand to get rid of" tile) )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: getBestMove
; Purpose: Returns advice on best move given the player number and valid trains
; Parameters: game object, valid trains, player number playing
; Algorithm: First, check if there is a best double
;				if so, return it
;			If not, move on to checking own train for marker
; Return Value: The best move in form of advice
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun getBestMove (game validTrains playerNumber)
	;first try to play doublz, otherewise move on to
	;step 2, checkOwnTrainPriority, where we see if our
	;own train has a marker on it that needs to be removed
	(let (
		(advice (getBestDouble game validTrains playerNumber 9) ) )
		(cond
			( (null advice)
				(checkOwnTrainPriority game validTrains playerNumber) )
			( (listp advice)
				advice )
			( t
				(checkOwnTrainPriority game validTrains playerNumber) )
		)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: checkOwnTrainPriority
; Purpose: Check if players own train has a marker as well as both the means and the
;			eligibility to play against it
; Parameters: game object, valid trains list, player number
; Algorithm: If player has a marker and ability to play against it, play the best possible
;				single against it
			;Otherwise, play the best possible single against whatever trains are available
; Return Value: the best move in form of advice
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun checkOwnTrainPriority (game validTrains playerNumber)
	;second step of AI priority chain: marker on own train
	;if there is one, and can play to it, recommend to do so
	; otherwise move on to step 3, which is highest singles
	(cond
		( (checkPlayAgainstMarker game validTrains playerNumber)
			;get the best single move on just own train
			(getBestSingle game (setNth playerNumber (list () () () ) t) playerNumber (getPriorityTiles) ) ) 
		( t
			;get the best single move wherever the heck
			(getBestSingle game validTrains playerNumber (getPriorityTiles) ) )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: checkPlayAgainstMarker
; Purpose: Returns true if player has both a marker on their train and the ability to
;			to play against it
; Parameters: game object, valid trains, player number
; Algorithm: return true IF player has marker on train AND players train is in valid
; trains list AND player has a tile in their hand that can be played against their train
; Return Value:  t or nil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun checkPlayAgainstMarker (game validTrains playerNumber)
	(and (and (playerHasMarker game playerNumber) (getNth playerNumber validTrains) ) (canPlayHandToTrain game (getHand game playerNumber) (getTrain game playerNumber) ) )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: getBestDouble
; Purpose: Find best doubles (descending order) to play againsrt valid trains. 
;				If unable to find a double to play, return nil
; Parameters: game object, valid trains player number
;				n: the number of pips on either side of the double, starting from
;				9 and going down to 0 (recursively)
; Algorithm: If n is less than 0, no playable doubles were found
;				If the players hand contains this double, return the best play for it
;				Otherwise, try the next best double
; Return Value: the best doubles play or NIL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun getBestDouble (game validTrains playerNumber n)
	(cond
		( (< n 0)
			() )
		( (listContains (getHand game playerNumber) (list n n) )
			(cond
				( (canPlayTileAnyWhere game (list n n) validTrains )
					(getBestTrainForTile game (list n n) validTrains playerNumber) )
				( t
					(getBestDouble game validTrains playerNumber (- n 1) ) )
			) )
		( t
			(getBestDouble game validTrains playerNumber (- n 1) ) )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: getBestSingle
; Purpose: Returns the best singles play that can be made
; Parameters: game object, valid trains, player number, list of tiles sorted by
;				priority e.g. descending order
; Algorithm:   Iterate recursively through the priorityTiles, removing one each
;				step of the way. If the player has that tile and can play it, return
;				the play. Since this function is only called afterno double is found,
;				and hasValidMove returned t before that, then we are guaranteed to
;				find a single to play
; Return Value: A valid,  best single to play to the best possible train
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun getBestSingle (game validTrains playerNumber priorityTiles)
	(cond
		( (null priorityTiles)
			;should be impossible but Just in case
			(format t "Fatal getBestSingle ~d ~d ~d ~d" game validTrains playerNumber priorityTiles) )
		( (and (listContains (getHand game playerNumber) (first priorityTiles) ) (canPlayTileAnyWhere game (first priorityTiles) validTrains) )
			(getBestTrainForTile game  (first priorityTiles) validTrains playerNumber) )
		( t
			(getBestSingle game validTrains playerNumber (rest priorityTiles) ) )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: getBestTrainForFile
; Purpose: After an ideal tile is picked, it may or may not be playable to more than
;			one train. This function returns a play on the best possible one
; Parameters: game object, tile to play, valid trains, player number
; Algorithm: if player has marker and they can play against it with this tile, thats
;					highest priority.
;			next it is checked against players own train
;			next it is checked against opponents train
;			lastly, it is assumed it must be played to mexican train
; Return Value: a list of (trainNumber tile)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun getBestTrainForTile (game tile validTrains playerNumber)
	(cond
		( (and (playerHasMarker game playerNumber) (and (getNth playerNumber validTrains) (canPlayTileToTrain game (getTrain game playerNumber) tile) ) )
			;we can play this tile to our own marked train, so do it
				(list playerNumber tile) )
		( (and (getNth playerNumber validTrains) (canPlayTileToTrain game (getTrain game playerNumber) tile) )
			;next play on our own
			(list playerNumber tile)  )
		( (and (getNth (- 3 playerNumber) validTrains) (canPlayTileToTrain game (getTrain game (- 3 playerNumber) ) tile) )
			(list (- 3 playerNumber) tile)  )
		( t
			;only other option is mexican
			(list 3 tile)  )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: canPlayHandToTrain
; Purpose: simply return true if any tile in this hand can be legally played to this train
; Parameters: game object, the hand, the train
; Algorithm: If hand is empty, return nil
;			If the first tile in hand can be played, return true
;			Otherwise, iterate recursively until hand is empty
; Return Value: t or nil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun canPlayHandToTrain (game hand train)
	(cond
		( (null hand)
			() )
		( (canPlayTileToTrain game train (first hand) )
			t )
		( t
			(canPlayHandToTrain game (rest hand) train) )
	)
)