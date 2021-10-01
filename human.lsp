(defun takeHumanTurn (game)
	(cond
		( (equal (getValidNumber 1 2 "(1) Save (2) Start Turn") ) 1)
			(saveGame game) )
		( t
			(startHumanTurn game (getOrphanTrains game) )
	)
)

;returns a list of bools 
(defun getOrphanTrains (game)
	(list
		(isDouble (getLast (getComputerTrain game) ) )
		(isDouble (getLast (getHumanTrain game) ) )
		(isDouble (getLast (getMexicanTrain game) ) )
	)	
)

(defun startHumanTurn (game startingOrphans)
	;synthesize a new list of bools from startingOrphans
	;and information regarding markers
	;	( (f or marker) t t) AND startingOrphans
	; is final list of eligible trains for rest of turn :)
	; rest is tile specific
	
)