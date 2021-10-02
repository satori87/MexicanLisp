(defun remMarker (train)
	(cond
		( (null train)
			train )
		( (equal (listp (first train) ) () )
			(rest train) )
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
	(OR (isDouble (first (remMarker (getComputerTrain game) ) ) ) (OR (isDouble (getLast (remMarker (getHumanTrain game) ) ) ) (isDouble (getLast (getMexicanTrain game) ) ) ) )
)


(defun getOrphanTrains (game)
	(list
		(isDouble (first (remMarker (getComputerTrain game) ) ) )
		(isDouble (getLast (remMarker (getHumanTrain game) ) ) )
		(isDouble (getLast (getMexicanTrain game) ) )
	)	
)