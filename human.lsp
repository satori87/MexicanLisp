(defun takeHumanTurn (game)
	(cond
		( (equal (getValidNumber 1 2 "(1) Save (2) Start Turn") 1)
			(saveGame game) )
		( t
			(startHumanTurn game (getValidHumanTrains game) ) )
	)
)

;returns a list of bools 
;if any orphans exist, only return orphaned trains 
(defun getValidHumanTrains (game)
	(cond
		( (isAnyOrphans game)
			(getValidHumanOrphanTrains game) )
		( t
			(list (listContains (getComputerHand game) "M") t t) )
	)
)


(defun isAnyOrphans (game)
	(OR (isDouble (first (remList (getComputerTrain game) "M") ) ) (OR (isDouble (getLast (remList (getHumanTrain game) "M") ) ) (isDouble (getLast (getMexicanTrain game) ) ) ) )
)

(defun getValidHumanOrphanTrains (game)
	(list
		(AND (listContains (getComputerHand game) "M") (isDouble (first (remList (getComputerTrain game) "M") ) ) )
		(isDouble (getLast (remList (getHumanTrain game) "M") ) )
		(isDouble (getLast (getMexicanTrain game) ) )
	)	
)

(defun startHumanTurn (game validTrains)
	;synthesize a new list of bools from startingOrphans
	;and information regarding markers
	;	( (f or marker) t t) AND startingOrphans
	; is final list of eligible trains for rest of turn :)
	; rest is tile specific
	(printListLn '"Valid Trains: " validTrains)
	game
)