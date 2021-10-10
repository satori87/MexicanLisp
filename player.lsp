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
; Function Name: 
; Purpose: 
; Parameters: 
; Algorithm: 
; Return Value: 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun remTile (hand tile)
	(remlist (remList hand tile) (reverseList tile) )
)

(defun getValidTrains (game playableTrains)
	(cond
		( (isAnyOrphans game)
			(getOrphanTrains game) )
		( t
			playableTrains )
	)
)

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