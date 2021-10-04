; STRATEGY
;  play doubles above all else
;  If marker on own train and can play to own train, do it
;  otherwise, play highest single

(defun askForHelp (game validTrains)
	;iterate through raw boneyard, seeing if that tile is in hand
	;if it is, call canPlayTileAnywhere
	;if that returns true, pick the most optimal train for that tile
	(let* (
		(advice (getBestMove game validTrains 2) )
		(trainNumber (getNth 1 advice ) )
		(tile (getNth 2 advice ) )
		)
		(format t "I recommend you play ~d to ~d because ~d " tile (getTrainName trainNumber) (getMoveReason trainNumber tile) ) (terpri)
	)
)

;move this to player, its modular
;use playernumber to determine which train is our own
(defun getBestMove (game validTrains playerNumber)
	;first try to play doublz
	(let (
		(advice (getBestDoubles game validTrains playerNumber) ) )
		(cond
			( (listp advice)
				advice )
			( (and (hasMarker (getTrain game playerNumber) ) (getNth playerNumber validTrains) )

		)
	)
)