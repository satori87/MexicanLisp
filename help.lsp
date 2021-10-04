; STRATEGY
;  play doubles above all else
;  If marker on own train and can play to own train, do it
;  otherwise, play highest single

(defun askForHelp (game validTrains)
	;iterate through raw boneyard, seeing if that tile is in hand
	;if it is, call canPlayTileAnywhere
	;if that returns true, pick the most optimal train for that tile
	(cond
		( (null (hasValidMove game (getHumanHand game) validTrains) )
			(princ "You have no valid move.") (terpri) )
		( t
			(let* (
				(advice (getBestMove game validTrains 2) )
				(trainNumber (getNth 1 advice ) )
				(tile (getNth 2 advice ) )
				)
				(format t "I recommend you play ~d to ~d because ~d " tile (getTrainName trainNumber) (getMoveReason trainNumber tile) ) (terpri)
			)
		)
	)
)

;move this to player, its modular
;use playernumber to determine which train is our own
(defun getBestMove (game validTrains playerNumber)
	;first try to play doublz, otherewise move on to
	;step 2, checkOwnTrainPriority, where we see if our
	;own train has a marker on it that needs to be removed
	(let (
		(advice (getBestDouble game validTrains playerNumber 9) ) )
		(cond
			( (listp advice)
				advice )
			( t
				(checkOwnTrainPriority game validTrains playerNumber) )
		)
	)
)

(defun checkPlayAgainstMarker (game validTrains playerNumber)
	(and (and (playerHasMarker game playerNumber) (getNth playerNumber validTrains) ) (canPlayHandToTrain game (getHand game playerNumber) (getTrain game playerNumber) ) )
)

(defun checkOwnTrainPriority (game validTrains playerNumber)
	;second step of AI priority chain: marker on own train
	;if there is one, and can play to it, recommend to do so
	; otherwise move on to step 3, which is highest singles
	(cond
		( (checkPlayAgainstMarker game validTrains playerNumber)
			;get the best single move on just own train
			(getBestSingle game (setNth (list () () () ) playerNumber t) playerNumber) ) 
		( t
			;get the best single move wherever the heck
			(getBestSingle game validTrains playerNumber (getPriorityTiles) ) )
	)
)

;iterate down to 0 trying every double against your hand
(defun getBestDouble (game validTrains playerNumber n)
	(cond
		( (listContains (getHand game playerNumber) (list n n) )
			(cond
				( (= n 0)
					() )
				( (canPlayTileAnyWhere game (list n n) validTrains )
					(getBestTrainForTile game (list n n) validTrains playerNumber) )
				( t
					(getBestDouble validTrains playerNumber (- n 1) ) )
			) )
		( t
			(getBestDouble validTrains playerNumber (- n 1) ) )
	)
)

(defun getBestSingle (game validTrains playerNumber priorityTiles)
	(cond
		( (null priorityTiles)
			;should be impossible but Just in case
			(princ "Fatal getBestSingle") )
		( (canPlayTileAnyWhere game (first priorityTiles) validTrains)
			(getBestTrainForTile game  (first priorityTiles) validTrains playerNumber) )
	)
)

(defun playTileToBestTrain (game tile validTrains playerNumber)
	;by this point any marker considerations are baked into
	;valid trains. all we need to do here is try to play in the 
	; following order: opponent train, own train, mexican train
	; keep in mind that we only know 
)

(defun canPlayHandToTrain (game hand train)
	;simply return true if any tile in this hand can be legally played to this train
	(cond
		(null hand)
			() )
		(canPlayTileToTrain game train (first hand) )
			t )
		( t
			(canPlayHandToTrain game (rest hand) train) )
	)
)