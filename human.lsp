(defun takeHumanTurn (game) 
	(showHumanTurnMenu)
	(cond
		( (hasValidMove game)
			(firstHumanMove game) )
		( t
			game )
	)
)

(defun showHumanTurnMenu game)
	(let(
		( input (getValidNumber 1 2 "(1) Save (2) Begin Turn") ) )
		(cond
			( (equal input 1)
				(save game) )
			( t
				() )
		)
	)
)