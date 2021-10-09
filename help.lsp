; STRATEGYaskforhelp
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
				(format t '"I recommend you play ~d to ~d because ~d " tile (getTrainName trainNumber) (getMoveReason game trainNumber tile 2) ) (terpri)
			)
		)
	)
)


(defun getComputerMove (game validTrains)
	;iterate through raw boneyard, seeing if that tile is in hand
	;if it is, call canPlayTileAnywhere
	;if that returns true, pick the most optimal train for that tile
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

(defun getMoveReason (game trainNumber tile playerNumber)
	(format t "getMoveReason ~d ~d ~d" trainNumber tile playerNumber) (terpri)
	(cond
		( (isDouble tile)
			(format nil '"~d is the highest playable double, allowing another turn" tile) )
		( (and (= trainNumber playerNumber) (playerHasMarker game playerNumber) )
			(format nil '"~d is the best tile playable to own train, which has a marker to consider" tile) )
		( t
			(format nil '"~d is the highest pip count tile in hand to get rid of" tile) )
	)
)

;move this to player, its modular
;use playernumber to determine which train is our own
(defun getBestMove (game validTrains playerNumber)
	(princ "getBestMove") (terpri)
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

(defun checkPlayAgainstMarker (game validTrains playerNumber)
	(princ "checkPlayAgainstMarker") (terpri)
	(and (and (playerHasMarker game playerNumber) (getNth playerNumber validTrains) ) (canPlayHandToTrain game (getHand game playerNumber) (getTrain game playerNumber) ) )
)

(defun checkOwnTrainPriority (game validTrains playerNumber)
	(princ "checkOwnTrainPriority") (terpri)
	;second step of AI priority chain: marker on own train
	;if there is one, and can play to it, recommend to do so
	; otherwise move on to step 3, which is highest singles
	(cond
		( (checkPlayAgainstMarker game validTrains playerNumber)
			;get the best single move on just own train
			(getBestSingle game (setNth (list () () () ) playerNumber t) playerNumber (getPriorityTiles) ) ) 
		( t
			;get the best single move wherever the heck
			(getBestSingle game validTrains playerNumber (getPriorityTiles) ) )
	)
)

;iterate down to 0 trying every double against your hand
(defun getBestDouble (game validTrains playerNumber n)
	(format t '"getBestDouble ~d" n) (terpri)
	(cond
		( (= n 0)
			() )
		( (listContains (getHand game playerNumber) (list n n) )
			(cond
				( (= n 0)
					() )
				( (canPlayTileAnyWhere game (list n n) validTrains )
					(getBestTrainForTile game (list n n) validTrains playerNumber) )
				( t
					(getBestDouble game validTrains playerNumber (- n 1) ) )
			) )
		( t
			(getBestDouble game validTrains playerNumber (- n 1) ) )
	)
)

(defun getBestSingle (game validTrains playerNumber priorityTiles)
	(format t '"getBestSingle ~d ~d" validTrains playerNumber) (terpri)
	(cond
		( (null priorityTiles)
			;should be impossible but Just in case
			(princ "Fatal getBestSingle") )
		( (and (listContains (getHand game playerNumber) (first priorityTiles) ) (canPlayTileAnyWhere game (first priorityTiles) validTrains) )
			(getBestTrainForTile game  (first priorityTiles) validTrains playerNumber) )
		( t
			(getBestSingle game validTrains playerNumber (rest priorityTiles) ) )
	)
)

(defun getValidTrainsForTile (game tile)
	(list (canPlayTileToTrain game (getComputerTrain game) tile) (canPlayTileToTrain game (getHumanTrain game) tile) (canPlayTileToTrain game (getMexicanTrain game) tile) )
)

(defun getBestTrainForTile (game tile validTrains playerNumber)
	(princ "getBestTrainForTile") (terpri)
	;by this point any marker considerations are baked into
	;valid trains. all we need to do here is try to play in the 
	; following order: opponent train, own train, mexican train
	; keep in mind that we only know 
	(cond
		( (and (playerHasMarker game playerNumber) (and (getNth playerNumber validTrains) (canPlayTileToTrain game (getTrain game playerNumber) tile) ) )
			;we can play this tile to our own marked train, so do it
				(princ "A5") (terpri)
				(list playerNumber tile) )
		( (and (isDouble tile) (and (getNth (- 3 playerNumber) validTrains) (canPlayTileToTrain game (getTrain game (- 3 playerNumber) ) tile) ) )
			(princ "A4") (terpri)
			(list (- 3 playerNumber) tile)  )
		( (and (getNth playerNumber validTrains) (canPlayTileToTrain game (getTrain game playerNumber) tile) )
			;next play on our own
			(princ "A3") (terpri)
			(list playerNumber tile)  )
		( (and (getNth (- 3 playerNumber) validTrains) (canPlayTileToTrain game (getTrain game (- 3 playerNumber) ) tile) )
			(format t "A2 ~d ~d ~d ~d" validTrains playerNumber (- 3 playerNumber) tile) (terpri)
			(list (- 3 playerNumber) tile)  )
		( t
			;only other option is mexican
			(princ "A1") (terpri)
			(list 3 tile)  )
	)
)

(defun canPlayHandToTrain (game hand train)
	(princ "canPlayHandToTrain") (terpri)
	;simply return true if any tile in this hand can be legally played to this train
	(cond
		( (null hand)
			() )
		( (canPlayTileToTrain game train (first hand) )
			t )
		( t
			(canPlayHandToTrain game (rest hand) train) )
	)
)