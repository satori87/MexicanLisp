(defun remMarker (train)
	(cond
		( (null train)
			train )
		( (equal (listp (getLast train) ) () )
			(allButLast train) )
		( t
			train)
	)
)

(defun hasMarker (train)
	(cond
		( (null train)
			() )
		( (equal (listp (first train) ) () )
			t )
		( (equal (listp (getLast train) ) () )
			t )
		( t
			() )
	)
)

(defun setMarker (train marker)
	(cond
		( (and marker (null (hasMarker train) ) )
			(append train (list 'M) ) )
		( (null marker)
			(remMarker train) )
		( t
			train )
	)
)

(defun isAnyOrphans (game)
	(OR (isDouble (getLast (remMarker (getComputerTrain game) ) ) ) (OR (isDouble (getLast (remMarker (getHumanTrain game) ) ) ) (isDouble (getLast (getMexicanTrain game) ) ) ) )
)


(defun getOrphanTrains (game)
	(list
		(isDouble (getLast (remMarker (getComputerTrain game) ) ) )
		(isDouble (getLast (remMarker (getHumanTrain game) ) ) )
		(isDouble (getLast (getMexicanTrain game) ) )
	)	
)

(defun getTrain (game trainNumber)
	(cond
		( (= trainNumber 1)
			(getComputerTrain game) )
		( (= trainNumber 2)
			(getHumanTrain game) )
		( (= trainNumber 3)
			(getMexicanTrain game) )
	)
)

(defun canPlayHandToTrain (game train hand)
	(cond
		( (null hand)
			() )
		( (canPlayTileToTrain game train (first hand) )
			t )
		(t
			(canPlayHandToTrain game train (rest hand) ) )
	)
)

; as usual, we can assume this train is oriented
; with engine on left, marker on right
; (left to right). we can also assume train has
; at least an engine. if train is null, it MUST be mexican,
; so use round number to fake a train with just the engine
(defun canPlayTileToTrain (game train tile)
	;if either of the values in tile matches the LAST value in train, return true
	(cond
		( (null train) 
			(canPlayTileToTrain game (list (getEngine (getRoundNumber game) ) ) tile) )
		( (= (getEndValue train ) (first tile) )
			t )
		( (= (getEndValue train ) (getLast tile) )
			t )
		( t
			nil )
	)
)

;returns list of (game tilePlayed trainNumberPlayed)
(defun finalizePlay (game trainNumber train tile playerNumber)
	;now just pick the right train to set
	(cond
		( (= trainNumber 1)
			(cond
				( (= playerNumber 1)
					(list (setComputerTrain game (remMarker train) ) tile) )
				(t
					(list (setComputerTrain game train) tile) )
			) )			
		( (= trainNumber 2)
			(cond
				( (= playerNumber 2)
					(list (setHumanTrain game (remMarker train) ) tile) )
				(t
					(list (setHumanTrain game train) tile) )
			) )		
		( (= trainNumber 3)
			(list (setMexicanTrain game train) tile) )
	)
)

(defun playTileToTrain (game trainNumber tile playerNumber)
	(format t "playing ~d to train ~d" tile trainNumber) (terpri)
	(let* ( (train (getTrain game trainNumber) )
			(marker (hasMarker train) )
		  )
		(cond
			( (equal (getEndValue train) (first tile) )
				(finalizePlay game trainNumber (setMarker (append (remMarker train) (list tile) ) marker) tile playerNumber) )
			( (equal (getEndValue train) (first (rest tile) ) )
				(finalizePlay game trainNumber (setMarker (append (remMarker train) (list (reverseList tile) ) ) marker) (reverseList tile) playerNumber) )
			( t
				(princ "Fatal error") (terpri)
				(quit) )
		)	
	)
)

(defun getEndValue (train)
	(getLast (getLast (remMarker train) ) )
)

(defun getTrainName (trainNumber)
	(cond
		( (= trainNumber 1)
			'"Computer Train" )
		( (= trainNumber 2)
			'"Human Train" )
		( (= trainNumber 3)
			'"Mexican Train" )		
	)
)