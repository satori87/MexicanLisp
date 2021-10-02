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
		( (= (getEndValue train) (first tile) )
			t )
		( (= (getEndValue train) (getLast tile) )
			t )
		( t
			() )
	)
)

(defun getEndValue (train)
	(getLast (getLast (remMarker train) ) )
)