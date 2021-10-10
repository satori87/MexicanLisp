;     ************************************************************
;     * Name:  Michael Whitlock                                  *
;     * Project:  Mexican Train (LISP)                           *
;     * Class:  OPL Fall 2021                                    *
;     * Date:  10/10/21                                          *
;     ************************************************************
;
;     player.lsp contains all functions universal to both players
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: endTurn
; Purpose: ends the players turn, announcing so and adjusting the player passed data
;			as necesarry, as well as placing marker if turn is passed without playing tile
; Parameters: game object, number of tiles played
; Algorithm: 
; Return Value: 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun endTurn (game tilesPlayed)
	(cond
		( (equal (getNextPlayer game) 'Computer)
			(princ "***Computer Ends Turn. Human Starts Turn***") (terpri)
			(cond
				( (> tilesPlayed 0) ;remove passed status, leave marker unchanged here
					(setComputerPassed (setNextPlayer game 'Human) nil ) )
				( t ;set passed to true and place marker
					(setComputerTrain (setComputerPassed (setNextPlayer game 'Human) t ) (setMarker (getComputerTrain game) t ) ) )
			) )
		( (equal (getNextPlayer game) 'Human)
			(format t "***Human Ends Turn. Computer Starts Turn***  Human played ~d tiles" tilesPlayed) (terpri)
			(cond
				( (> tilesPlayed 0) ;remove passed status, leave marker unchanged here
					(setHumanPassed (setNextPlayer game 'Computer) nil ) )
				( t ;set passed to true and place marker
					(setHumanTrain (setHumanPassed (setNextPlayer game 'Computer) t ) (setMarker (getHumanTrain game) t ) ) )
			) )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: tallyHand
; Purpose: Adds up all pips in an entire hand and returns the number
; Parameters: a hand
; Algorithm: iterate through entire hand recursively, using getTileValue to get count of
;			each tile
; Return Value: pip count of entire hand as number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tallyHand (hand)
	(cond 
		( (null hand)
			0 )
		( t
			(+ (getTileValue (first hand) ) (tallyHand (rest hand) ) ) )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: remTile
; Purpose: removes the specified tile from the specified hand
; Parameters: hand, tile
; Algorithm: Attempt to remove both the tile and its reverse
;				Since double 9 set contains no duplicates, this is valid
; Return Value: the hand without the tile in it
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun remTile (hand tile)
	(remlist (remList hand tile) (reverseList tile) )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: getValidTrains
; Purpose: Returns a list of booleans corresponding to which trains will be legal
;			to play to this turn with regards to Orphan doubles. If no orphan doubles
;			exist, then the supplied playableTrains argument is returned
; Parameters: game object, playableTrains
; Algorithm: If any orphan doubles exist, return the list of trains with
;				orphan doubles
; Return Value: list of 3 booleans representing validity of computer, human, mexican trains
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun getValidTrains (game playableTrains)
	(cond
		( (isAnyOrphans game)
			(getOrphanTrains game) )
		( t
			playableTrains )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: hasValidMove
; Purpose: Returns t or nil for whether or not the given hand can legally play to any
;			of the specified validTrains
; Parameters: game object, the player hand, eligible trains
; Algorithm: iterate through entire hand recursively, checking each tile for playability
;			return t if any are found
; Return Value: t or nil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun hasValidMove (game hand validTrains)
	(cond
		( (null hand)
			() )
		( t
			(or (canPlayTileAnywhere game (first hand) validTrains) (hasValidMove game (rest hand) validTrains) ) )
	)
)

(defun canPlayTileAnywhere (game tile validTrains)
	(cond
		( (and (getNth 1 validTrains) (canPlayTileToTrain game (getComputerTrain game) tile) )
			t )
		( (and (getNth 2 validTrains) (canPlayTileToTrain game (getHumanTrain game) tile) )
			t )
		( (and (getNth 3 validTrains) (canPlayTileToTrain game (getMexicanTrain game) tile) )
			t )
		( t
			() )
	)
)

(defun getHand (game playerNumber)
	(cond
		( (= playerNumber 1)
			(getComputerHand game) )
		( (= playerNumber 2)
			(getHumanHand game) )
	)
)

(defun playerHasMarker (game playerNumber)
	(hasMarker (getTrain game playerNumber) )
)